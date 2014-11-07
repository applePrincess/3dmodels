{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | DirectX Object File (.x) Parser
module Graphics.Model.DirectX (
  loadXOF,
  parseXOF,
  reverseHand,

  XOF(..),
  DxMesh(..),
  DxMaterial(..)
  ) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString as B
import Data.Traversable
import Data.Word
import Linear

-- | DirectX Object File
data XOF = XOF
	{ _xofFormat :: B.ByteString
	, _xofTemplates :: [(B.ByteString, [B.ByteString])]
	, _xofHeader :: (Word16, Word16, Word32) -- ^ major minor flags
	, _xofMesh :: DxMesh
	-- , _xofFrames :: [(frame_name, [(mesh_name, DxMesh)])]
	-- , _xofAnimationSets :: [DxAnimationSet :: (frame_name, [DxKeyframe])]
	} deriving Show

data DxMesh = DxMesh
	{ _dxVertices :: [V3 Float]
	, _dxFaces :: [Either (V4 Word32) (V3 Word32)]
	, _dxIndexes :: [Word32]
	, _dxMaterials :: [DxMaterial]
	, _dxNormals :: [V3 Float]
	, _dxNormalIndexes :: [Either (V4 Word32) (V3 Word32)]
	, _dxTexCoords :: [V2 Float]
	, _dxVertexColors ::  [(Word32, V4 Float)]
	} deriving Show

data DxMaterial = DxMaterial
	{ _dxFaceColor :: V4 Float
	, _dxPower :: Float
	, _dxSpecular :: V3 Float
	, _dxEmissive :: V3 Float
	, _dxTexturePath :: Maybe B.ByteString
	} deriving Show

--data DxKeyframe =
--	  DxRotate Int Quaternion
--	| DxScale Int (V3 Float)
--	| DxTranslate Int (V3 Float)
--	| DxApplyMatrix Int M44 Float


loadXOF :: FilePath -> IO XOF
loadXOF path = parseXOF <$> B.readFile path

parseXOF :: B.ByteString -> XOF
parseXOF bs =
	case feed (parse parseXMesh bs) "" of
		Done _ result -> result
		result -> error $ "parseXOF: Invalid format:\n" ++ show result

-- | Convert to right handed system.
reverseHand :: XOF -> XOF
reverseHand xof@XOF{_xofMesh=DxMesh{..}, ..} =
	xof { _xofMesh = (_xofMesh xof) {
		_dxVertices = fmap rhVtx _dxVertices,
		_dxFaces = fmap rhFace _dxFaces,
		_dxNormals = fmap rhVtx _dxNormals,
		_dxNormalIndexes = fmap rhFace _dxNormalIndexes
	}}
	where rhVtx (V3 x y z) = V3 x y (-z)
	      rhFace (Right (V3 a b c)) = Right $ V3 b a c
	      rhFace (Left (V4 a b c d)) = Left $ V4 a d c b

parseXMesh :: Parser XOF
parseXMesh = XOF
	<$> string "xof 0302txt 0064" <* skipSpace -- \r\n
	<*> many (templateDecl <* skipSpace)
	<*> headerDecl <* skipSpace
	<*> meshDecl

takeIdent :: Parser B.ByteString
takeIdent = A.takeWhile (inClass "a-zA-Z0-9_")

-- | e.g. template Header { field1; field2; ... }
templateDecl = try $
	(,) <$> (string "template" >> skipSpace >> takeIdent <* skipSpace) -- name
	<*> braces '{' '}' (skipSpace >> many (fieldDecl <* skipSpace)) -- fields

fieldDecl =
	braces '<' '>' (A.takeWhile (/= '>'))
	<|> braces '[' ']' (string "..." <|> takeIdent)
	<|> A.takeWhile (inClass "a-zA-Z0-9_ []") <* char ';'

float = realToFrac <$> signed double

getVector :: (Applicative a, Traversable a) => Parser (a Float)
getVector = sequenceA $ pure getFloatField

-- | e.g. <aaa> => braces '<' '>' (string "aaa")
braces l r p = char l *> p <* char r

semicolon = char ';' <* skipSpace
comma = char ','
maybeComma = (comma <|> return ' ') >> skipSpace

getIntField = decimal <* semicolon
getFloatField = float <* semicolon
getStringField = braces '"' '"' (A.takeWhile (/= '"')) <* semicolon

getVectorField :: (Applicative a, Traversable a) => Parser (a Float)
getVectorField = getVector <* semicolon

getArray :: Parser a -> Int -> Parser [a]
getArray parser len = count len (parser <* maybeComma) <* semicolon

section name p = do
	string name
	skipSpace
	braces '{' '}' (skipSpace >> p)

headerDecl :: Parser (Word16, Word16, Word32)
headerDecl = section "Header" $ do
	major <- fromIntegral <$> getIntField
	minor <- fromIntegral <$> getIntField
	flags <- fromIntegral <$> getIntField
	return (major, minor, flags)

meshDecl :: Parser DxMesh
meshDecl = section "Mesh" $ do
	vertices <- getIntField >>= getArray getVector
	faces <- getIntField >>= getArray meshFace
	(indexes, materials) <- option ([], []) meshMaterialList <* skipSpace
	(normals, faceNormals) <- option ([], []) meshNormals <* skipSpace
	DxMesh vertices faces indexes materials normals faceNormals
		<$> option [] meshTextureCoords <* skipSpace
		<*> option [] meshVertexColors <* skipSpace

meshFace = fmap Left meshFace4 <|> fmap Right meshFace3
meshFace4 = do
	char '4' >> semicolon
	sequenceA $ V4 (decimal <* comma) (decimal <* comma)
		(decimal <* comma) (decimal <* char ';')
meshFace3 = do
	char '3' >> semicolon
	sequenceA $ V3 (decimal <* comma)
		(decimal <* comma) (decimal <* char ';')

meshMaterialList = section "MeshMaterialList" $ do
	nMaterials <- getIntField
	indexes <- getIntField >>= getArray faceIndex
	semicolon
	materials <- count nMaterials (materialDecl <* skipSpace)
	return (indexes, materials)

faceIndex :: Parser Word32
faceIndex = decimal

materialDecl = section "Material" $ do
	faceColor <- getVectorField
	power <- getFloatField
	specularColor <- getVectorField
	emissiveColor <- getVectorField
	texFile <- optional textureFilename
	skipSpace
	return $ DxMaterial faceColor power specularColor emissiveColor texFile

textureFilename = section "TextureFilename" getStringField

meshNormals = section "MeshNormals" $ do
	normals <- getIntField >>= getArray getVector
	faceNormals <- getIntField >>= getArray meshFace
	return (normals, faceNormals)

meshTextureCoords = section "MeshTextureCoords" $
	getIntField >>= getArray getVector

meshVertexColors = section "MeshVertexColors" $
	getIntField >>= getArray indexedColor

indexedColor = (,) <$> fmap fromIntegral getIntField <*> getVector

