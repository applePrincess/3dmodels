-- | MikuMikuDance (MMD) model/motion reader.
-- This module provides unpacking interfaces for MMD 3D models and their motions,
-- both traditional PMD and newer PMX format are fully supported.
-- 
-- > model <- loadMMD "博麗霊夢_n＋式/博麗霊夢_n＋式.pmd" :: IO MMD
-- 
-- > motion <- loadVMD "Motion_BadApple/BadApple_Rside_20110206.vmd" :: IO VMD
module Graphics.Model.MikuMikuDance (
  loadMMD,
  decodeMMD,
  unpackMMD,
  loadVMD,
  decodeVMD,
  unpackVMD,
  module Graphics.Model.MikuMikuDance.Types
) where
import Graphics.Model.MikuMikuDance.Types
import Graphics.Model.MikuMikuDance.Loader

