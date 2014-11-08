module Graphics.Model.MikuMikuDance.Types (
  VertexIx,
  TexIx,
  BoneIx,
  MorphIx,
  MaterialIx,
  BodyIx,
  MMD(..),
  MMDVertex(..),
  MMDWeight(..),
  MMDMaterial(..),
  MMDBone(..),
  MMDIK(..),
  MMDMorph(..),
  MMDMorphOffest(..),
  MMDGroup(..),
  MMDRigidBody(..),
  MMDJoint(..),
  MMDSoftBody(..),

  VMD(..),
  BoneKeyframe(..),
  MorphKeyframe(..),
  CameraKeyframe(..),
  LightKeyframe(..),
  ShadowKeyframe(..),
  ControlKeyframe(..),

  VPD
  ) where
import qualified Data.ByteString as B
import Data.Int
import Data.Word
import Linear


-- * PMX (Polygon Model eXtended)

type VertexIx = Int32
type TexIx = Int32
type BoneIx = Int32
type MorphIx = Int32
type MaterialIx = Int32
type BodyIx = Int32

-- | PMX 2.1 spec made by Kyockhook-P (極北P)
data MMD = MMD
	{ mmdFormat :: Float
	-- ^ 1.0: PMD, 2.0/2.1:PMX
	, mmdCharCode :: String
	-- ^ UTF-16, UTF-8, Shift-JIS
	, mmdExtUVs :: Word8 -- ^ in [0..4]
	, mmdNameJa :: B.ByteString
	, mmdName :: B.ByteString
	, mmdCommentJa :: B.ByteString
	, mmdComment :: B.ByteString
	, mmdVertices :: [MMDVertex]
	, mmdPlanes :: [V3 VertexIx]
	-- ^ ABC(Triangle), /Since 2.1/: AAA(Point), ABA(Line)
	, mmdTextures :: [B.ByteString]
	, mmdAltToonTex :: [B.ByteString]
	-- ^ length: 0(PMX) or 10(PMD, if exists)
	, mmdMaterials :: [MMDMaterial]
	, mmdBones :: [MMDBone]
	, mmdMorphs :: [MMDMorph]
	, mmdGroups :: [MMDGroup]
	, mmdRigidBodies :: [MMDRigidBody]
	, mmdJoints :: [MMDJoint]
	, mmdSoftBodies :: [MMDSoftBody]
	-- ^ /Since 2.1/
	} deriving Show

data MMDVertex = MMDVertex
	{ vPosition :: V3 Float
	, vNormal :: V3 Float
	, vUV :: V2 Float
	, vExtUVs :: [V4 Float]
	, vWeight :: MMDWeight
	, vEdgeScale :: Float
	} deriving Show

data MMDWeight =
	  BDEF1 BoneIx
	-- | Weight of the first bone
	| BDEF2 (V2 BoneIx) Float
	-- | Weight of each bone
	| BDEF4 (V4 BoneIx) (V4 Float)
	-- | BDEF2 + SDEF-C(x,y,z) + SDEF-R0(x,y,z) + SDEF-R1(x,y,z)
	| SDEF (V2 BoneIx) Float (M33 Float)
	-- | /Since 2.1/ DualQuaternion
	| QDEF (V4 BoneIx) (V4 Float)
	deriving Show

data MMDMaterial = MMDMaterial
	{ mNameJa :: B.ByteString
	, mName :: B.ByteString
	, mDiffuse :: V4 Float -- ^ RGBA
	, mSpecular :: V3 Float -- ^ RGB
	, mSpecularFactor :: Float
	, mAmbient :: V3 Float -- ^ RGB
	-- | 1:cullface=both, 2:drop shadow, 4:render to self shadow map,
	-- 8:render to self shadow, 16:draw edges,
	-- /Since 2.1/ => 32:vertex color, 64:As Points, 128:As Lines
	, mOptions :: Word8
	, mEdgeColor :: V4 Float -- ^ RGBA
	-- | /Since 2.1/ Point size (See mOptions)
	, mEdgeSize :: Float
	, mTexture :: TexIx
	, mSphereTexture :: TexIx
	-- | 0: None, 1: Multiply, 2: Add, 3: Use subtexture
	, mSphereMode :: Word8
	-- | Indexed texture >= 0, -10 <= shered toon texture (0-9) - 10 < 0
	, mToonTexture :: TexIx
	, mComment :: B.ByteString
	-- | The number of planes * 3
	, mNumVertices :: Word32
	} deriving Show

data MMDBone = MMDBone
	{ bNameJa :: B.ByteString
	, bName :: B.ByteString
	, bPosition :: V3 Float
	, bParent :: BoneIx
	, bTransformDepth :: Int
	, bFlags :: Word16
	, bLInk :: Either (V3 Float) BoneIx -- ^ offset or bone index
	, bFollow :: Maybe (BoneIx, Float)
	, bAxis :: Maybe (V3 Float)
	, bLocalAxis :: Maybe (V3 Float, V3 Float)
	, bExternalParent :: Maybe Int
	, bIK :: Maybe MMDIK
	} deriving Show

data MMDIK = MMDIK
	{ ikTarget :: BoneIx
	, ikLoopCount :: Int
	, ikRotLimitPerLoop :: Float
	, ikLinks :: [(BoneIx, Maybe (V3 Float, V3 Float))]
	} deriving Show

data MMDMorph = MMDMorph
	{ mphNameJa :: B.ByteString
	, mphName :: B.ByteString
	, mphUICtrl :: Word8
	, mphOffsets :: [MMDMorphOffest]
	} deriving Show

data MMDMorphOffest =
	  GroupMorph MorphIx Float
	| VertexMorph VertexIx (V3 Float) -- ^ offset
	| BoneMorph BoneIx (V3 Float) (V4 Float)
	| UVMorph VertexIx (V4 Float)
	| UV1Morph VertexIx (V4 Float)
	| UV2Morph VertexIx (V4 Float)
	| UV3Morph VertexIx (V4 Float)
	| UV4Morph VertexIx (V4 Float)
	| MaterialMorph MaterialIx Word8 (V4 Float) (V3 Float) Float (V3 Float) (V4 Float) Float (V4 Float) (V4 Float) (V4 Float)
	-- ^ 0:*, 1:+. Diffuse, Specular, SpecularFactor, Ambient, EdgeColor, EdgeSize, TextureFactors, SphereTexFactors, ToonTexFactors.
	| FlipMorph MorphIx Float
	-- ^ /Since 2.1/
	| ImpulseMorph BodyIx Bool (V3 Float) (V3 Float)
	-- ^ /Since 2.1/ isLocal, velocity, torque
	deriving Show

data MMDGroup = MMDGroup
	{ gNameJa :: B.ByteString
	, gName :: B.ByteString
	, gIsReserved :: Bool
	, gElements :: [Either BoneIx MorphIx]
	} deriving Show

data MMDRigidBody = MMDRigidBody
	{ rbNameJa :: B.ByteString
	, rbName :: B.ByteString
	, rbRelated :: BoneIx
	, rbGroup :: Word8
	, rbNoCollisionGroup :: Word16
	, rbShape :: Word8 -- ^ 0:Ball, 1:Box, 2:Capsule
	, rbSize :: V3 Float -- ^ (x,y,z)
	, rbPosition :: V3 Float -- ^ (x,y,z)
	, rbRotation :: V3 Float -- ^ (x,y,z)
	, rbMass :: Float
	, rbSpeedAttenuation :: Float
	, rbRotAttenuation :: Float
	, rbRepulsion :: Float
	, rbFriction :: Float
	, rbComputeMode :: Word8 -- ^ 0: static, 1: dynamic, 2: both
	} deriving Show

data MMDJoint = MMDJoint
	{ jNameJa :: B.ByteString
	, jName :: B.ByteString
	, jointType :: Word8
	, jBone1 :: BodyIx
	, jBone2 :: BodyIx
	, jPosition :: V3 Float
	, jRotation :: V3 Float
	, jPosUpperBound :: V3 Float
	, jPosLowerBound :: V3 Float
	, jRotUpperBound :: V3 Float
	, jRotLowerBound :: V3 Float
	, jPosSpringConst :: V3 Float
	, jRotSpringConst :: V3 Float
	} deriving Show

-- | /Since 2.1/
data MMDSoftBody = MMDSoftBody
	{ sbNameJa :: B.ByteString
	, sbName :: B.ByteString
	, sbShape :: Word8
	-- ^ 0:TriMesh, 1:Rope
	, sbRelated :: MaterialIx
	, sbGroup :: Word8
	, sbNoCollisionGroup :: Word16
	, sbOptions :: Word8
	-- ^ 1:generateBendingConstraints(B-Link), 2:generateClusters, 4:randomizeConstraints
	, sbBLinkDistance :: Int
	, sbNumClusters :: Int
	, sbMass :: Float
	, sbCollisionMargin :: Float
	, sbAeroModel :: Int
	-- ^ 0:V_Point, 1:V_TwoSided, 2:V_OneSided, 3:F_TwoSided, 4:F_OneSided
	, sbConfig :: V3 (V4 Float)
	-- ^ (VCF DP DG LF, PR VC DF MT, CHR KHR SHR AHR)
	, sbCluster :: V2 (V3 Float)
	-- ^ (SRHR_CL SKHR_CL SSHR_CL, SR_SPLT_CL SK_SPLT_CL SS_SPLT_CL)
	, sbIteration :: V4 Int
	-- ^ V_IT P_IT D_IT C_IT
	, sbMaterial :: V3 Float
	-- ^ LST AST VST
	, sbAnchorRigidBodies :: [(BodyIx, VertexIx, Bool)]
	-- ^ True: Enable Near mode
	, sbPinVertices :: [VertexIx]
	} deriving Show


-- * VMD (Vocaloid Motion Data)

-- VMD strings are encoded in CP932
-- | Vocaloid Motion Data
data VMD = VMD
	{ vmdModelName :: B.ByteString -- ^ CP932
	, boneMotion :: [BoneKeyframe]
	, morphMotion :: [MorphKeyframe]
	, cameraMotion :: [CameraKeyframe]
	, lightMotion :: [LightKeyframe]
	, shadowMotion :: [ShadowKeyframe]
	, controlMotion :: [ControlKeyframe]
	} deriving Show

data BoneKeyframe = BoneKeyframe
	{ bkName :: B.ByteString
	, bkFrame :: Word32
	, bkPosition :: V3 Float -- ^ XYZ or (0,0,0)
	, bkRotation :: V4 Float -- ^ Quaternion XYZW or (0,0,0,1)
	, bkInterpolation :: [Int] -- XXX description
	} deriving Show

data MorphKeyframe = MorphKeyframe
	{ moName :: B.ByteString
	, moFrame :: Word32
	, moFace :: Float
	} deriving Show

data CameraKeyframe = CameraKeyframe
	{ ckFrame :: Word32
	, ckDistance :: Float
	, ckPosition :: V3 Float
	, ckRotation :: V3 Float
	, ckBezier :: [Int] -- ^ bezier int[24]
	, ckViewingAngle :: Word32 -- ^ in degree
	, ckPerspective :: Bool -- 0:Enable, 1:Disable
	} deriving Show

data LightKeyframe = LightKeyframe
	{ lkFrame :: Word32
	, lkColor :: V3 Float
	, lkPosition :: V3 Float
	} deriving Show

data ShadowKeyframe = ShadowKeyframe
	{ skFrame :: Word32
	, skType :: Int
	, skDistance :: Float -- ^ 0.1 - (dist * 0.00001)
	} deriving Show

data ControlKeyframe = ControlKeyframe
	{ ctFrame :: Word32
	, ctModelVisibility :: Bool
	, ctIKCapability :: [(B.ByteString, Bool)]
	} deriving Show


-- * VPD

-- | Vocaloid Pose Data
-- (name of a Bone, position of the bone, quaternion of the bone)
type VPD = (B.ByteString, V3 Float, V4 Float)


-- * VAC

-- | Vocaloid Accessory 
-- (name of an accessory, .x filename, position, eular angle, name of the bound bone)
type VAC = (String, FilePath, Float, V3 Float, V3 Float, String)

