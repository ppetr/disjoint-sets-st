{-# LANGUAGE BangPatterns, FlexibleContexts #-}
{- | Imperative disjoint sets data structure.
 - Uses mutable arrays with path compression and union by rank to achieve nearly constant amortized time complexity.
 - (It's actually the inverted Ackermann function, which is less than 5 for all remotely possible sizes.)
 - Optimized to be used with unboxed arrays of integers.
 -}
module Data.DisjointSet
    (
    -- * Core functions.
      DSet
    , singletons
    , find
    , union
    , classes
    -- * Utility functions.
    , singletonsIO
    , singletonsST
    , sameClass
    )
where
import Control.Monad
import Control.Monad.ST
import Data.Ix
import Data.Array.MArray
import Data.Array.IO (IOUArray)
import Data.Array.ST (STUArray)

-- | A collection of disjoint sets on 'Int's backed by a
-- mutable array of type @a@.
data DSet a = DSet { classesAr :: !(a () Int), parents :: !(a Int Int), ranks :: !(a Int Int) }

-- | Creates a new disjoint set structure with the specified bounds.
-- Calling @mkset (i,j)@ creates a collection of singleton sets indexed
-- by numbers from @i@ to @j@ (inclusive).
singletons :: (MArray a Int m) => (Int, Int) -> m (DSet a)
singletons bs = liftM3 DSet (newArray ((),()) (rangeSize bs)) (newListArray bs rng) (newListArray bs rng)
  where rng = range bs

-- | A convenience function for creating an efficient, 'ST'-based array.
singletonsST :: (Int, Int) -> ST s (DSet (STUArray s))
singletonsST = singletons

-- | A convenience function for creating an efficient, 'IO'-based array.
singletonsIO :: (Int, Int) -> IO (DSet IOUArray)
singletonsIO = singletons

getParent :: (MArray a Int m) => DSet a -> Int -> m Int
getParent (DSet _ ps _) = readArray ps
{-# INLINE getParent #-}

setParent :: (MArray a Int m) => DSet a -> Int -> Int -> m ()
setParent (DSet _ ps _) = writeArray ps
{-# INLINE setParent #-}

getRank :: (MArray a Int m) => DSet a -> Int -> m Int
getRank (DSet _ _ rs) = readArray rs
{-# INLINE getRank #-}

setRank :: (MArray a Int m) => DSet a -> Int -> Int -> m ()
setRank (DSet _ _ rs) = writeArray rs
{-# INLINE setRank #-}

-- | Returns the identifier of the subset a given element is in.
find :: (MArray a Int m) => DSet a -> Int -> m Int
find ds = f
  where
    f i = do
        i' <- getParent ds i
        if i' == i
          then return i
          else do
            j <- f i'
            setParent ds i j
            return j
{-# SPECIALIZE find :: DSet (STUArray s) -> Int -> ST s Int #-}
{-# SPECIALIZE find :: DSet IOUArray     -> Int -> IO Int #-}

-- | Returns @True@ iff the given two elements belong to the same class.
-- In many cases this function is preferred over 'find'.
sameClass :: (MArray a Int m) => DSet a -> Int -> Int -> m Bool
sameClass ds x y = liftM2 (==) (find ds x) (find ds y)
{-# INLINE sameClass #-}

-- | Joins the classes of given two elements.
-- Returns 'True' if the two classes were merged (i.e. were distinct before),
-- 'False' otherwise.
union :: (MArray a Int m) => DSet a -> Int -> Int -> m Bool
union ds x y = do
    xf <- find ds x
    yf <- find ds y
    if xf == yf
      then return False
      else do
        xr <- getRank ds xf
        yr <- getRank ds yf
        case compare xr yr of
            LT  -> setParent ds xf yf
            GT  -> setParent ds yf xf
            EQ  -> do
                    setParent ds yf xf
                    setRank ds xf (xr + 1)
        let car = classesAr ds
        liftM (subtract 1) (readArray car ()) >>= writeArray car ()
        return True
{-# SPECIALIZE union :: DSet (STUArray s) -> Int -> Int -> ST s Bool #-}
{-# SPECIALIZE union :: DSet IOUArray     -> Int -> Int -> IO   Bool #-}

classes :: (MArray a Int m) => DSet a -> m Int
classes (DSet c _ _) = readArray c ()
{-# SPECIALIZE classes :: DSet (STUArray s) -> ST s Int #-}
{-# SPECIALIZE classes :: DSet IOUArray     -> IO   Int #-}
