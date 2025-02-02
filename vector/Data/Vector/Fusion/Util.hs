-- |
-- Module      : Data.Vector.Fusion.Util
-- Copyright   : (c) Roman Leshchinskiy 2009
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : portable
-- 
-- Fusion-related utility types
--

module Data.Vector.Fusion.Util (
  Id(..), Box(..),

  delay_inline, delayed_min
) where

-- | Identity monad
newtype Id a = Id { unId :: a }

instance Functor Id where
  fmap f (Id x) = Id (f x)

instance Applicative Id where
  pure     = Id
  -- f (a -> b) -> f a -> f b
  Id f <*> Id x  = Id (f x)

instance Monad Id where
  return     = Id
  Id x >>= f = f x

-- | Box monad
data Box a = Box { unBox :: a }

instance Functor Box where
  fmap f (Box x) = Box (f x)

instance Applicative Box where
  pure = Box
  Box f <*> Box x = Box (f x)

instance Monad Box where
  return      = Box
  Box x >>= f = f x

-- | Delay inlining a function until late in the game (simplifier phase 0).
delay_inline :: (a -> b) -> a -> b
{-# INLINE [0] delay_inline #-}
delay_inline f = f

-- | `min` inlined in phase 0
delayed_min :: Int -> Int -> Int
{-# INLINE [0] delayed_min #-}
delayed_min m n = min m n

