{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Rank2Types #-}
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
module Data.Vector.Map.Fusion
  ( merge
  , insert
  , unstream
  ) where

import Data.Vector.Fusion.Stream.Monadic as Stream
-- import Data.Vector.Fusion.Bundle as Stream


import           Data.Vector.Generic 
import           Data.Vector.Generic.Mutable (MVector)
import           Data.Vector.Generic.Base ( Vector, Mutable )
import Control.Monad.ST ( ST, runST )
import Control.Monad.Primitive

data New v a = New (forall s. ST s (Mutable v s a))


-- -- | /O(1)/ Unsafe convert a mutable vector to an immutable one without
-- -- copying. The mutable vector may not be used after this operation.
-- unsafeFreeze
--   :: (PrimMonad m, Vector v a) => Mutable v (PrimState m) a -> m (v a)
-- {-# INLINE unsafeFreeze #-}
-- unsafeFreeze = basicUnsafeFreeze

-- -- | Construct a vector from a monadic initialiser.
-- new :: Vector v a => New v a -> v a
-- {-# INLINE_STREAM new #-}
-- new m = m `seq` runST (unsafeFreeze =<< New.run m)

-- | Create a new mutable vector and fill it with elements from the monadic
-- stream. The vector will grow exponentially if the maximum size of the stream
-- is unknown.
munstream :: (PrimMonad m, MVector v a) => Stream m a -> m (v (PrimState m) a)
{-# INLINE_STREAM munstream #-}
munstream s = case upperBound (Stream.size s) of
               Just n  -> munstreamMax     s n
               Nothing -> munstreamUnknown s

munstreamMax
  :: (PrimMonad m, MVector v a) => Stream m a -> Int -> m (v (PrimState m) a)
{-# INLINE munstreamMax #-}
munstreamMax s n
  = do
      v <- INTERNAL_CHECK(checkLength) "munstreamMax" n
           $ unsafeNew n
      let put i x = do
                       INTERNAL_CHECK(checkIndex) "munstreamMax" i n
                         $ unsafeWrite v i x
                       return (i+1)
      n' <- Stream.foldM' put 0 s
      return $ INTERNAL_CHECK(checkSlice) "munstreamMax" 0 n' n
             $ unsafeSlice 0 n' v

munstreamUnknown
  :: (PrimMonad m, MVector v a) => Stream m a -> m (v (PrimState m) a)
{-# INLINE munstreamUnknown #-}
munstreamUnknown s
  = do
      v <- unsafeNew 0
      (v', n) <- MStream.foldM put (v, 0) s
      return $ INTERNAL_CHECK(checkSlice) "munstreamUnknown" 0 n (length v')
             $ unsafeSlice 0 n v'
  where
    {-# INLINE_INNER put #-}
    put (v,i) x = do
                    v' <- unsafeAppend1 v i x
                    return (v',i+1)

-- | Create a new mutable vector and fill it with elements from the 'Stream'.
-- The vector will grow exponentially if the maximum size of the 'Stream' is
-- unknown.
unstream'' :: (PrimMonad m, MVector v a) => Stream f a -> m (v (PrimState m) a)
-- NOTE: replace INLINE_STREAM by INLINE? (also in unstreamR)
{-# INLINE_STREAM unstream #-}
unstream'' s = munstream (Stream.liftStream s)

unstream' :: Vector v a => Stream m a -> New v a
{-# INLINE_STREAM unstream #-}
unstream' s = s `seq` New (unstream'' s)

-- | /O(n)/ Construct a vector from a 'Stream'
unstream :: Vector v a => Stream m a -> v a
{-# INLINE unstream #-}
unstream s = Data.Vector.Generic.new (unstream' s)


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
merge :: (Monad m, Ord k) => Stream m (k, a) -> Stream m (k, a) -> Stream m (k, a)
merge (Stream stepa sa0) (Stream stepb sb0) = Stream step (MergeStart sa0 sb0) where
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

insert :: (Monad m, Ord k) => k -> a -> Stream m (k, a) -> Stream m (k, a)
insert k c (Stream stepa sa0) = Stream step (Searching sa0) where
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
