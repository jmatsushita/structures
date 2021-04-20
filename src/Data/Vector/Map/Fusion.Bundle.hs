{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- COLA fusion internals
--
-----------------------------------------------------------------------------
module Data.Vector.Map.Fusion.Bundle
  ( merge
  , insert
  ) where

-- import Data.Vector.Fusion.Bundle as Bundle
import Data.Vector.Fusion.Util (Id)
import Data.Vector.Fusion.Stream.Monadic (Stream(..), Step(..))
import Data.Vector.Fusion.Bundle.Monadic (Bundle(..))
import Data.Vector.Fusion.Bundle.Size as Bundle

-- | The state for 'Stream' fusion that is used by 'mergeStreamsWith'.
--
-- This form permits cancellative addition.
data MergeState sa sb i a
  = MergeL sa sb i a
  | MergeR sa sb i a
  | MergeLeftEnded sb
  | MergeRightEnded sa
  | MergeStart sa sb

-- | This is the internal stream fusion combinator used to merge streams for addition.
merge :: (Ord k) => Bundle Id v (k, a) -> Bundle Id v (k, a) -> Bundle Id v (k, a)
merge 
  (Bundle (Stream stepa sa0) (Stream stepCa chunksa0) _ sizea) 
  (Bundle (Stream stepb sb0) (Stream stepCb chunksb0) _ sizeb) 
    = Bundle 
      (Stream step (MergeStart sa0 sb0)) 
      (Stream stepC (MergeChunkStart chunksa0 chunksb0)) 
      Nothing 
      (sizea + sizeb) 
    where
  step (MergeStart sa sb) = do
    r <- stepa sa
    return $ case r of
      Yield (i, a) sa' -> Skip (MergeL sa' sb i a)
      Skip sa'         -> Skip (MergeStart sa' sb)
      Done             -> Skip (MergeLeftEnded sb)
  step (MergeL sa sb i a) = do
    r <- stepb sb
    return $ case r of
      Yield (j, b) sb' -> case compare i j of
        LT -> Yield (i, a) (MergeR sa sb' j b)
        EQ -> Yield (i, a) (MergeStart sa sb')
        GT -> Yield (j, b) (MergeL sa sb' i a)
      Skip sb' -> Skip (MergeL sa sb' i a)
      Done     -> Yield (i, a) (MergeRightEnded sa)
  step (MergeR sa sb j b) = do
    r <- stepa sa
    return $ case r of
      Yield (i, a) sa' -> case compare i j of
        LT -> Yield (i, a) (MergeR sa' sb j b)
        EQ -> Yield (i, a) (MergeStart sa' sb)
        GT -> Yield (j, b) (MergeL sa' sb i a)
      Skip sa' -> Skip (MergeR sa' sb j b)
      Done     -> Yield (j, b) (MergeLeftEnded sb)
  step (MergeLeftEnded sb) = do
    r <- stepb sb
    return $ case r of
      Yield (j, b) sb' -> Yield (j, b) (MergeLeftEnded sb')
      Skip sb'         -> Skip (MergeLeftEnded sb')
      Done             -> Done
  step (MergeRightEnded sa) = do
    r <- stepa sa
    return $ case r of
      Yield (i, a) sa' -> Yield (i, a) (MergeRightEnded sa')
      Skip sa'         -> Skip (MergeRightEnded sa')
      Done             -> Done
  {-# INLINE [0] step #-}
{-# INLINE [1] merge #-}

-- | The state for 'Stream' fusion that is used by 'mergeStreamsAnd'.
--
-- This form permits cancellative addition.
data InsertState sa ia
  = Searching sa
  | Holding sa ia
  | Found sa
  | Over

insert :: (Ord k) => k -> a -> Bundle Id v (k, a) -> Bundle Id v (k, a)
insert k c (Bundle (Stream stepa sa0) chunks vecs sizea) = Bundle (Stream step (Searching sa0)) chunks vecs (sizea + 1) where
  step (Searching sa) = do
    r <- stepa sa
    return $ case r of
      Yield ia sa' -> case compare (fst ia) k of
        LT -> Yield ia (Searching sa')
        EQ -> Yield (k, c) (Found sa')
        GT -> Yield (k, c) (Holding sa' ia)
      Skip sa' -> Skip (Searching sa')
      Done     -> Yield (k, c) Over
  step (Holding sa ia) = return $ Yield ia (Found sa)
  step (Found sa) = do
    r <- stepa sa
    return $ case r of
      Yield p sa' -> Yield p (Found sa')
      Skip sa'    -> Skip (Found sa')
      Done        -> Done
  step Over = return Done
  {-# INLINE [0] step #-}
{-# INLINE [1] insert #-}
