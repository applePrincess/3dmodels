{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
-- | OBJ is a widely used simple 3D model format made for TAV.
-- The Advanced Visualizer (TAV), a 3D graphics software package,
-- was the flagship product of Wavefront Technologies from the 1980s
-- until the 1990s.
-- <https://en.wikipedia.org/wiki/The_Advanced_Visualizer>

module Graphics.Model.Obj (
  loadOBJ,
  parseOBJ,
  --reverseHandOBJ,
  loadMtl,
  parseMtl,

  ObjMesh(..),
  ObjFace(..),
  ObjMaterial(..)
  ) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString as B
import Data.Int
import Linear

data ObjMesh = ObjMesh
	{ _objGroup :: String
	, _objMaterial :: Maybe (FilePath, String)
	, _objVertices :: [V3 Float]
	, _objUVs :: [V2 Float]
	, _objNormals :: [V3 Float]
	, _objFaces :: [[ObjFace]]
	} deriving Show

data ObjFace = ObjFace
	{ _objVertexIx :: Int32
	, _objTextureIx :: Int32
	, _objNormalIx :: Int32
	} deriving Show

data ObjMaterial = ObjMaterial
	{ _objName :: String
	, _objAmbient :: V3 Float
	, _objDiffuse :: V3 Float
	, _objSpecular :: V3 Float
	, _objShineness :: Float
	} deriving Show

loadOBJ :: FilePath -> IO [ObjMesh]
loadOBJ path = parseOBJ <$> B.readFile path

parseOBJ :: B.ByteString -> [ObjMesh]
parseOBJ bs =
	case feed (parse parseMeshes bs) "" of
		Done _ result -> result
		result -> error $ "parseOBJ: Invalid format:\n" ++ show result

comments = do
	many (skipSpace >> char '#' >> A.takeWhile (/= '\n'))
	skipSpace

parseMeshes :: Parser [ObjMesh]
parseMeshes = do
	path <- comments *> option "" parseMtllib <* comments
	many1 (parseMesh path <* comments)

parseMesh :: String -> Parser ObjMesh
parseMesh path = do
	group <- option "default" parseG <* comments
	mtl1 <- optional parseUsemtl <* comments
	vertices <- many1 (parseV <* comments)
	uvs <- many (parseVt <* comments)
	normals <- many (parseVn <* comments)
	mtl2 <- optional parseUsemtl <* comments
	faces <- many (parseF <* comments)
	let mtl = fmap (path,) (maybe mtl2 Just mtl1)
	return $ ObjMesh group mtl vertices uvs normals faces

getString, parseMtllib, parseG, parseUsemtl :: Parser String
getString = many1 (satisfy (/= '\n'))
parseMtllib = string "mtllib " >> getString
parseG = char 'g' >> sp >> getString
parseUsemtl = string "usemtl " >> getString

parseV = char 'v' >> float3
parseVt = char 'v' >> char 't' >> float2
parseVn = char 'v' >> char 'n' >> float3
parseF = char 'f' >> many indexes

float = realToFrac <$> signed double
{-# INLINE float #-}

sp = char ' '
{-# INLINE sp #-}

float3 = V3
	<$> (sp *> float)
	<*> (sp *> float)
	<*> (sp *> float)
{-# INLINE float3 #-}

float2 = V2
	<$> (sp *> float)
	<*> (sp *> float)
{-# INLINE float2 #-}

index = option 0 decimal
{-# INLINE index #-}

indexes = ObjFace
	<$> (sp       *> index)
	<*> (char '/' *> index)
	<*> (char '/' *> index)
{-# INLINE indexes #-}

loadMtl :: FilePath -> IO ObjMaterial
loadMtl path = parseMtl <$> B.readFile path

parseMtl :: B.ByteString -> ObjMaterial
parseMtl bs =
	case feed (parse parseMtl' bs) "" of
		Done _ result -> result
		result -> error $ "parseMtl: Invalid format:\n" ++ show result

parseMtl' :: Parser ObjMaterial
parseMtl' = do
	comments
	mtl <- string "newmtl " >> getString <* comments
	amb <- string "Ka" >> float3 <* comments
	diff <- string "Kd" >> float3 <* comments
	spec <- string "Ks" >> float3 <* comments
	shine <- string "Ns " >> float
	-- XXX map_Kd
	return $ ObjMaterial mtl amb diff spec shine

