{-# LANGUAGE OverloadedStrings #-}
module Graphics.Model.MikuMikuDance.Loader where
import Control.Applicative
import Control.Monad
import Data.Int
import Data.Bits
import Data.Packer
import qualified Data.ByteString as B
import Graphics.Model.MikuMikuDance.Types
import Linear
import Unsafe.Coerce


getFloat :: Unpacking Float -- Performance hack (may not work on your devices!)
getFloat = unsafeCoerce <$> getWord32LE

get2 :: Unpacking a -> Unpacking (V2 a)
get2 get = V2 <$> get <*> get

get3 :: Unpacking a -> Unpacking (V3 a)
get3 get = V3 <$> get <*> get <*> get

get4 :: Unpacking a -> Unpacking (V4 a)
get4 get = V4 <$> get <*> get <*> get <*> get

getV2 :: Unpacking (V2 Float)
getV2 = get2 getFloat

getV3 :: Unpacking (V3 Float)
getV3 = get3 getFloat

getV4 :: Unpacking (V4 Float)
getV4 = get4 getFloat

getWord8Int, getWord16Int, getWord32Int :: Unpacking Int
getWord8Int = fromIntegral <$> getWord8
getWord16Int = fromIntegral <$> getWord16LE
getWord32Int = fromIntegral <$> getWord32LE

-- Left handed system -> Right handed system
ltrTrans (V3 x y z) = V3 x y (-z)
ltrEular (V3 x y z) = V3 (-x) (-y) z
ltrQuat (V4 t i j k) = V4 (-t) i j k
getPos = fmap ltrTrans getV3
getEular = fmap ltrEular getV3
getQuat = fmap ltrQuat getV4


-- * PMX

loadMMD :: FilePath -> IO MMD 
loadMMD path = decodeMMD <$> B.readFile path

decodeMMD :: B.ByteString -> MMD
decodeMMD bs = runUnpacking unpackMMD bs

unpackMMD :: Unpacking MMD
unpackMMD = do
	magic <- getWord32BE
	case magic of
		0x506d6400 -> unpackPMD
		0x504d5820 -> unpackPMX
		_ -> fail "Not a MMD Model"

getUnsignedIndexUnpacker :: Unpacking (Unpacking Int32)
getUnsignedIndexUnpacker = do
	size <- getWord8
	return $ case size of
		1 -> fromIntegral <$> getWord8
		2 -> fromIntegral <$> getWord16LE
		4 -> fromIntegral <$> getWord32LE

getIndexUnpacker :: Unpacking (Unpacking Int32)
getIndexUnpacker = do
	size <- getWord8
	return $ case size of
		1 -> ((fromIntegral :: Int8 -> Int32) . fromIntegral) <$> getWord8
		2 -> ((fromIntegral :: Int16 -> Int32) . fromIntegral) <$> getWord16LE
		4 -> fromIntegral <$> getWord32LE

getTextBuf :: Unpacking B.ByteString
getTextBuf = getWord32Int >>= getBytesCopy

-- Polygon Model Extended
unpackPMX :: Unpacking MMD
unpackPMX = do
	version <- getFloat
	size <- getWord8
	when ((version /= 2.0 && version /= 2.1) || size /= 8) $
		fail "Not PMX 2.0/2.1"
	
	-- PMX Header
	-- 0: UTF-16, 1: UTF-8
	isUTF16 <- getWord8
	let charset 0 = "UTF-16"
	    charset 1 = "UTF-8"
	numExtUVs <- getWord8
	getVertexIndex <- getUnsignedIndexUnpacker
	getTextureIndex <- getIndexUnpacker
	getMaterialndex <- getIndexUnpacker
	getBoneIndex <- getIndexUnpacker
	getMorphIndex <- getIndexUnpacker
	getBodyIndex <- getIndexUnpacker
	modelName <- getTextBuf
	modelNameEn <- getTextBuf
	comment <- getTextBuf
	commentEn <- getTextBuf

	-- Vertex
	numVertices <- getWord32Int
	vertices <- replicateM numVertices $ do
		pos <- getPos
		normal <- getPos
		uv <- getV2
		extV4s <- replicateM (fromIntegral numExtUVs) getV4
		weightFormat <- getWord8
		w <- case weightFormat of
			0 -> BDEF1 <$> getBoneIndex
			1 -> BDEF2 <$> get2 getBoneIndex <*> getFloat
			2 -> BDEF4 <$> get4 getBoneIndex <*> getV4
			-- SDEF need left<->right handed system transformation?
			3 -> SDEF <$> get2 getBoneIndex <*> getFloat <*> get3 getPos
			4 -> QDEF <$> get4 getBoneIndex <*> getV4
		-- edgeScalingFactor
		esf <- getFloat
		return $ MMDVertex pos normal uv extV4s w esf

	-- Plane
	numPlanes <- getWord32Int
	planes <- replicateM (numPlanes `div` 3) $ do
		V3 p q r <- get3 getVertexIndex
		return $ V3 q p r -- from left to right
	
	-- Texture
	numTextures <- getWord32Int
	textures <- replicateM numTextures getTextBuf
	
	-- Material
	numMaterials <- getWord32Int
	materials <- replicateM numMaterials $ do
		name <- getTextBuf
		nameEn <- getTextBuf
		diffuse <- getV4
		specular <- getV3
		shininess <- getFloat
		ambient <- getV3
		renderFlags <- getWord8
		edgeColor <- getV4
		edgeSize <- getFloat
		tex <- getTextureIndex
		sphereTex <- getTextureIndex
		sphereMode <- getWord8
		toonFlag <- getWord8
		toon <- case toonFlag of
			0 -> getTextureIndex
			1 -> (\x -> fromIntegral x - 10) <$> getWord8
		script <- getTextBuf
		verticesCount <- getWord32LE
		return $ MMDMaterial name nameEn diffuse specular shininess ambient
			renderFlags edgeColor edgeSize tex sphereTex sphereMode toon
			script verticesCount

	-- Bone
	numBones <- getWord32Int
	bones <- replicateM numBones $ do
		name <- getTextBuf
		nameEn <- getTextBuf
		pos <- getPos
		parent <- getBoneIndex
		transDepth <- getWord32Int
		flags <- getWord16LE
		link <- if flags .&. 1 == 0
			then Left <$> getV3
			else Right <$> getBoneIndex
		following <- if flags .&. 0x300 /= 0
			then Just <$> ((,) <$> getBoneIndex <*> getFloat)
			else return Nothing
		axis <- if flags .&. 0x400 /= 0
			then Just <$> getV3
			else return Nothing
		local <- if flags .&. 0x800 /= 0
			then Just <$> ((,) <$> getPos <*> getPos)
			else return Nothing
		external <- if flags .&. 0x2000 /= 0
			then Just <$> getWord32Int
			else return Nothing
		ik <- if flags .&. 0x20 /= 0
			then do
				ix <- getBoneIndex
				lc <- getWord32Int
				maxRot <- getFloat
				numlink <- getWord32Int
				links <- replicateM numlink $ do
					ix <- getBoneIndex
					ranged <- getWord8
					limit <- if ranged == 1
						then Just <$> ((,) <$> getEular <*> getEular)
						else return Nothing
					return (ix, limit)
				return . Just $ MMDIK ix lc maxRot links
			else return Nothing
		return $ MMDBone name nameEn pos parent transDepth flags link
			following axis local external ik

	-- Morph
	numMorphs <- getWord32Int
	morphs <- replicateM numMorphs $ do
		name <- getTextBuf
		nameEn <- getTextBuf
		facial <- getWord8
		morphType <- getWord8
		let getMorphOffset = case morphType of
			0 -> GroupMorph <$> getMorphIndex <*> getFloat
			1 -> VertexMorph <$> getVertexIndex <*> getPos
			2 -> BoneMorph <$> getBoneIndex <*> getPos <*> getQuat
			3 -> UVMorph <$> getVertexIndex <*> getV4
			4 -> UV1Morph <$> getVertexIndex <*> getV4
			5 -> UV2Morph <$> getVertexIndex <*> getV4
			6 -> UV3Morph <$> getVertexIndex <*> getV4
			7 -> UV4Morph <$> getVertexIndex <*> getV4
			8 -> MaterialMorph <$> getMaterialndex <*> getWord8
				<*> getV4 <*> getV3 <*> getFloat
				<*> getV3 <*> getV4 <*> getFloat
				<*> getV4 <*> getV4 <*> getV4
			9 -> FlipMorph <$> getMorphIndex <*> getFloat
			10 -> ImpulseMorph <$> getBodyIndex <*> ((/= 0) <$> getWord8)
				<*> getPos <*> getEular
		numOffsets <- getWord32Int
		offsets <- replicateM numOffsets getMorphOffset
		return $ MMDMorph name nameEn facial offsets

	-- Group
	numGroups <- getWord32Int
	groups <- replicateM numGroups $ do
		name <- getTextBuf
		nameEn <- getTextBuf
		isSpecial <- getWord8
		count <- getWord32Int
		elements <- replicateM count $ do
			kind <- getWord8
			case kind of
				0 -> Left <$> getBoneIndex
				1 -> Right <$> getMorphIndex
		return $ MMDGroup name nameEn (isSpecial /= 0) elements

	-- Rigid Body
	numRigidBodies <- getWord32Int
	bodies <- replicateM numRigidBodies $
		MMDRigidBody <$> getTextBuf <*> getTextBuf <*> getBoneIndex
			<*> getWord8 <*> getWord16LE <*> getWord8
			<*> getV3 <*> getPos <*> getEular
			<*> getFloat <*> getFloat <*> getFloat
			<*> getFloat <*> getFloat <*> getWord8

	-- Joint
	numJoints <- getWord32Int
	joints <- replicateM numJoints $
		MMDJoint <$> getTextBuf <*> getTextBuf <*> getWord8
			<*> getBodyIndex <*> getBodyIndex
			<*> getPos <*> getEular
			<*> getPos <*> getPos <*> getEular <*> getEular
			<*> getPos <*> getEular

	-- Soft Body
	numSoftBodies <- getWord32Int
	softBodies <- replicateM numSoftBodies $
		MMDSoftBody <$> getTextBuf <*> getTextBuf <*> getWord8
			<*> getMaterialndex <*> getWord8 <*> getWord16LE
			<*> getWord8 <*> getWord32Int <*> getWord32Int
			<*> getFloat <*> getFloat <*> getWord32Int
			<*> get3 getV4 <*> get2 getV3
			<*> get4 getWord32Int <*> getV3
			<*> (getWord32Int >>= flip replicateM (
				(,,) <$> getBodyIndex <*> getVertexIndex
					<*> ((== 1) <$> getWord8)))
			<*> (getWord32Int >>= flip replicateM getVertexIndex)

	-- Sums up!
	return $ MMD version (charset isUTF16) numExtUVs
		modelName modelNameEn comment commentEn
		vertices planes textures [] materials bones morphs groups bodies
		joints softBodies

-- get fixed length C string terminating NUL byte
getFixedStr :: Int -> Unpacking B.ByteString
getFixedStr l = B.takeWhile (/= 0) <$> getBytesCopy l

-- Polygon Model Data
unpackPMD :: Unpacking MMD
unpackPMD = do
	unpackSetPosition 3
	version <- getFloat
	when (version /= 1.0) $
		fail "Not PMD 1.0"
	
	-- PMD Header
	name <- getFixedStr 20
	comment <- getFixedStr 256
	
	-- Vertex
	numVertices <- getWord32Int
	vertices <- replicateM numVertices $ do
		v <- MMDVertex <$> getPos <*> getPos <*> getV2
		weight <- BDEF2 <$> get2 getBoneIndex <*> getPercent
		edgeFlag <- getWord8
		let edgeScale = fromIntegral (1 - edgeFlag)
		return $ v [] weight edgeScale

	-- Plane
	numPlanesX3 <- getWord32Int
	planes <- replicateM (numPlanesX3 `div` 3) $ do
		V3 p q r <- get3 getVertexIndex
		return $ V3 q p r

	-- Material
	numMaterials <- getWord32Int
	materials <- forM [0 .. numMaterials - 1] $ \i -> do
		diffuse <- getV4
		specularity <- getFloat
		specular <- getV3
		ambient <- getV3
		toonIx <- fromIntegral <$> getWord8 -- -1:0.bmp, 0..9:[1..10].bmp
		edgeFlag <- getWord8 -- 1 to disable
		numVertices <- getWord32LE
		texturePath <- getFixedStr 20
		let sphereMode =
			-- "tex.bmp*sphere.sph" => Multiply (MMD 5.12-)
			-- "tex.bmp*sphere.spa" => Plus (MMD 5.12-)
			-- "tex.bmp/sphere.sph" => Multiply (MMD 5.11)
			-- "tex.bmp" or "sphere.sph" => None (MMD 5.09-)
			if 42 `B.elem` texturePath -- or 47 `elem` texturePath
			then if ".spa" `B.isSuffixOf` texturePath
				then 2 -- Add
				else 1 -- Mul
			else 0 -- None
		let i' = fromIntegral i
		return $ MMDMaterial texturePath ""
			diffuse specular specularity ambient
			((1 - edgeFlag) * 16) -- draw edge
			(V4 0 0 0 1) 1.0 (i' * 2) (i' * 2 + 1) sphereMode
			(toonIx - 10) B.empty numVertices

	-- Texture
	let split x y | mSphereMode x == 0 = mNameJa x : mNameJa x : y
	    split x y = t1 : B.tail t2 : y
	    	where (t1,t2) = B.break (== 42) (mNameJa x)
	let textures = foldr split [] materials
	
	-- Bone
	numBones <- getWord16Int
	bones <- replicateM numBones $ do
		name <- getFixedStr 20
		parent <- getBoneIndex
		tailBone <- getBoneIndex
		-- 0:回転 1:回転と移動 2:IK 3:不明 4:IK影響下
		-- 5:回転影響下 6:IK接続先 7:非表示 8:捻り 9:回転運動
		typ <- getWord8
		ik <- getBoneIndex
		headPos <- getPos
		return $ MMDBone name "" headPos parent
			0 0 -- XXX ?
			(Right 0) Nothing --XXX follow link
			Nothing Nothing Nothing Nothing

	-- IK
	numIKBones <- getWord16Int
	ikbones <- replicateM numIKBones $ do
		bone <- getBoneIndex
		target <- getBoneIndex
		ikLen <- getWord8Int
		iter <- getWord16Int
		rotLimit <- (* 4) <$> getFloat
		links <- replicateM ikLen $
			(\x -> (x, Nothing)) <$> getBoneIndex
		return $ (bone, MMDIK target iter rotLimit links)
	let bones' = zipWith (\ i bone ->
		bone { bIK = lookup i ikbones }) [0..] bones

	-- Face
	numFaces <- getWord16Int
	faces <- replicateM numFaces $ do
		name <- getFixedStr 20
		numVertices <- getWord32Int
		facial <- getWord8
		-- 0:Base, 1:Brow, 2: Eye, 3: Lip, 4:Other
		-- XXX 'base' should be ignored (0,0,0)???
		vMorph <- replicateM numVertices $
			VertexMorph <$> fmap fromIntegral getWord32LE <*> getPos
		return $ MMDMorph name "" facial vMorph
		
	faceListLen <- getWord8Int
	faceList <- replicateM faceListLen getMorphIndex

	-- Group
	boneGroupsLen <- getWord8Int
	boneGroups <- replicateM boneGroupsLen (getFixedStr 50)
	let bGsEn = replicate boneGroupsLen ""
	numGroupedBones <- getWord32Int
	groupedBones <- replicateM numGroupedBones $
		(,) <$> getBoneIndex <*> getWord8

	-- Ext - English support since MMD 4.03
	empty <- isEmpty
	haveEnNames <- if empty then return False else (== 1) <$> getWord8
	(nameEn, commentEn, bones, faces, bGsEn) <-
		if not haveEnNames
		then return ("", "", bones, faces, bGsEn)
		else do
			name' <- getFixedStr 20
			comment' <- getFixedStr 256
			bones' <- forM bones $ \bone -> do
					nameEn <- getFixedStr 20
					return bone { bName = nameEn }
			faces' <- forM faces $ \morph -> do
					nameEn <- getFixedStr 20
					return morph { mphName = nameEn }
			bGsEn' <- replicateM (boneGroupsLen - 1) (getFixedStr 50)
			return (name', comment', bones', faces', bGsEn')

	-- UTF-16
	let root = "R\NULo\NULo\NULt\NUL"
	let rootGroup = MMDGroup root root True [Left 0]
	let faceGroup = MMDGroup "h\136\197`" "E\NULx\NULp\NUL" True
			(map Right faceList)
	let groups = rootGroup : faceGroup :
		zipWith3 (\i j e ->
			MMDGroup j e False $
				map (Left . fst) $
					filter ((== i).snd) groupedBones
		) [1..] boneGroups bGsEn
		-- group 0 ('center') is reserved for the root bone
	
	-- toon texture list since MMD 4.03
	toonTexList <- if empty then return []
		else replicateM 10 (getFixedStr 100)

	-- Ext - Bullet Physics
	--    Rigid Body
	numRigidBodies <- getWord32Int
	bodies <- replicateM numRigidBodies $
		MMDRigidBody <$> getFixedStr 20 <*> pure "" <*> getBoneIndex
			<*> getWord8 <*> getWord16LE <*> getWord8
			<*> getV3 <*> getPos <*> getEular
			<*> getFloat <*> getFloat <*> getFloat
			<*> getFloat <*> getFloat <*> getWord8
	--    Joint
	numJoints <- getWord32Int
	joints <- replicateM numJoints $
		MMDJoint <$> getFixedStr 20 <*> pure "" <*> pure 0
			<*> getBodyIndex <*> getBodyIndex
			<*> getPos <*> getEular
			<*> getPos <*> getPos <*> getEular <*> getEular
			<*> getPos <*> getEular
	
	-- Sums up!
	return $ MMD 1 "Shift-JIS" 0 name nameEn comment commentEn
		vertices planes textures toonTexList materials bones'
		faces groups bodies joints []
	where
		toInt16 = fromIntegral
		toInt32 = fromIntegral :: Int16 -> Int32
		getBoneIndex = toInt32 . toInt16 <$> getWord16LE
		getPercent = (/100) . fromIntegral <$> getWord8
		getVertexIndex = fromIntegral <$> getWord16LE
		getMorphIndex = getVertexIndex
		getBodyIndex = fromIntegral <$> getWord32LE


-- * VMD

loadVMD :: FilePath -> IO VMD
loadVMD path = decodeVMD <$> B.readFile path

decodeVMD :: B.ByteString -> VMD
decodeVMD bs = runUnpacking unpackVMD bs

unpackVMD :: Unpacking VMD
unpackVMD = do
	magic <- getBytes 30
	when (magic /= "Vocaloid Motion Data 0002\0\0\0\0\0") $
		-- MMD 3.0+ required.
		fail "Not Vocaloid Motion Data"
	
	-- If the content is camera, light, and self shadow,
	-- model name should be "カメラ・照明\0on Data"
	name <- getFixedStr 20

	numBoneKeyframes <- getWord32Int
	boneM <- replicateM numBoneKeyframes $
		BoneKeyframe <$> getFixedStr 15 <*> getWord32LE <*> getPos
			<*> getQuat <*> replicateM 64 (fromIntegral <$> getWord8)
	
	numMorphKeyframes <- getWord32Int
	morphM <- replicateM numMorphKeyframes $
		MorphKeyframe <$> getFixedStr 15 <*> getWord32LE <*> getFloat

	numCameraKeyframes <- getWord32Int
	cameraM <- replicateM numCameraKeyframes $
		CameraKeyframe <$> getWord32LE <*> getFloat
			<*> getPos <*> getEular
			<*> replicateM 24 (fromIntegral <$> getWord8)
			<*> getWord32LE <*> ((== 0) <$> getWord8)

	numLightKeyframes <- getWord32Int
	lightM <- replicateM numLightKeyframes $
		LightKeyframe <$> getWord32LE <*> getV3 <*> getPos

	-- After MMDv6.19 --
	empty <- isEmpty
	shadowM <- if empty then return []
	else do
		numSelfShadowKeyframes <- getWord32Int
		replicateM numSelfShadowKeyframes $
			ShadowKeyframe <$> getWord32LE
				<*> (fromIntegral <$> getWord8) <*> getFloat
	
	-- Since MMDv7.40 --
	empty <- isEmpty
	ctrlM <- if empty then return []
	else do
		numCtrlKeyframes <- getWord32Int
		replicateM numCtrlKeyframes $
			ControlKeyframe <$> getWord32LE <*> ((== 1) <$> getWord8)
				<*> (getWord32Int >>= \count ->
					replicateM count
					((,) <$> getFixedStr 20 <*> ((== 1) <$> getWord8)))

	-- Sums up!
	return $ VMD name boneM morphM cameraM lightM shadowM ctrlM

