{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.Ord.Prim
-- Copyright   :  (c) Jacob Leach, 2022
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO
--
-- @since 1.0.0
module Data.Ord.Prim
  ( -- * Ordering#
    Ordering# (EQ#, GT#, LT#),
    fromOrdering,
    toOrdering,
    toInt#,
    unsafeFromInt#,

    -- * Eq#
    Eq# ((==#), (/=#)),

    -- * Ord#
    Ord# (compare#, (>#), (>=#), (<#), (<=#)),
  )
where

import Data.Bool.Prim (Bool#)
import Data.Bool.Prim qualified as Bool

import GHC.Exts
  ( Addr#,
    Char#,
    Double#,
    Float#,
    Int#,
    Int16#,
    Int32#,
    Int8#,
    Word#,
    Word16#,
    Word32#,
    Word8#,
  )
import GHC.Exts qualified as GHC
import GHC.Types (TYPE)

-- Ordering# -------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
newtype Ordering# = Ordering# Int#

-- | TODO
--
-- @since 1.0.0
pattern EQ# :: Ordering#
pattern EQ# <-
  (\_ -> Ordering# 0# -> Ordering# 0#)
  where
    EQ# = Ordering# 0#

-- | TODO
--
-- @since 1.0.0
pattern GT# :: Ordering#
pattern GT# = Ordering# 1#

-- | TODO
--
-- @since 1.0.0
pattern LT# :: Ordering#
pattern LT# = Ordering# -1#

{-# COMPLETE EQ#, GT#, LT# #-}

-- | @since 1.0.0
instance Eq# Ordering# where
  Ordering# x# ==# Ordering# y# = Bool.unsafeFromInt# (x# GHC.==# y#)
  {-# INLINE (==#) #-}

  Ordering# x# /=# Ordering# y# = Bool.unsafeFromInt# (x# GHC./=# y#)
  {-# INLINE (/=#) #-}

-- | @since 1.0.0
instance Ord# Ordering# where
  compare# (Ordering# x#) (Ordering# y#) = Ordering# ((x# GHC.># y#) GHC.-# (x# GHC.<# y#))
  {-# INLINE compare# #-}

  Ordering# x# ># Ordering# y# = Bool.unsafeFromInt# (x# GHC.># y#)
  {-# INLINE (>#) #-}

  Ordering# x# >=# Ordering# y# = Bool.unsafeFromInt# (x# GHC.>=# y#)
  {-# INLINE (>=#) #-}

  Ordering# x# <# Ordering# y# = Bool.unsafeFromInt# (x# GHC.<# y#)
  {-# INLINE (<#) #-}

  Ordering# x# <=# Ordering# y# = Bool.unsafeFromInt# (x# GHC.<=# y#)
  {-# INLINE (<=#) #-}

-- | TODO
--
-- @since 1.0.0
fromOrdering :: Ordering -> Ordering#
fromOrdering x = Ordering# (GHC.dataToTag# x GHC.-# 1#)

-- | TODO
--
-- @since 1.0.0
toOrdering :: Ordering# -> Ordering
toOrdering (Ordering# x#) = GHC.tagToEnum# (1# GHC.+# x#)

-- | TODO
--
-- @since 1.0.0
toInt# :: Ordering# -> Int#
toInt# (Ordering# x#) = x#

-- | TODO
--
-- @since 1.0.0
unsafeFromInt# :: Int# -> Ordering#
unsafeFromInt# = Ordering#

-- Eq# -------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
class Eq# (a :: TYPE r) where
  infix 4 ==#, /=#

  -- | TODO
  --
  -- @since 1.0.0
  (==#) :: a -> a -> Bool#

  -- | TODO
  --
  -- @since 1.0.0
  (/=#) :: a -> a -> Bool#

-- Eq# - Addr# -----------------------------------------------------------------

-- | @since 1.0.0
instance Eq# Addr# where
  x# ==# y# = Bool.unsafeFromInt# (GHC.eqAddr# x# y#)
  {-# INLINE (==#) #-}

  x# /=# y# = Bool.unsafeFromInt# (GHC.neAddr# x# y#)
  {-# INLINE (/=#) #-}

-- Eq# - Bool# -----------------------------------------------------------------

-- | @since 1.0.0
instance Eq# Bool# where
  x# ==# y# = Bool.eq# x# y#
  {-# INLINE (==#) #-}

  x# /=# y# = Bool.ne# x# y#
  {-# INLINE (/=#) #-}

-- Eq# - Char# -----------------------------------------------------------------

-- | @since 1.0.0
instance Eq# Char# where
  x# ==# y# = Bool.unsafeFromInt# (GHC.eqChar# x# y#)
  {-# INLINE (==#) #-}

  x# /=# y# = Bool.unsafeFromInt# (GHC.neChar# x# y#)
  {-# INLINE (/=#) #-}

-- Eq# - Int#, Int8#, Int16#, Int32# -------------------------------------------

-- | @since 1.0.0
instance Eq# Int# where
  x# ==# y# = Bool.unsafeFromInt# (x# GHC.==# y#)
  {-# INLINE (==#) #-}

  x# /=# y# = Bool.unsafeFromInt# (x# GHC./=# y#)
  {-# INLINE (/=#) #-}

-- | @since 1.0.0
instance Eq# Int8# where
  x# ==# y# = Bool.unsafeFromInt# (GHC.eqInt8# x# y#)
  {-# INLINE (==#) #-}

  x# /=# y# = Bool.unsafeFromInt# (GHC.neInt8# x# y#)
  {-# INLINE (/=#) #-}

-- | @since 1.0.0
instance Eq# Int16# where
  x# ==# y# = Bool.unsafeFromInt# (GHC.eqInt16# x# y#)
  {-# INLINE (==#) #-}

  x# /=# y# = Bool.unsafeFromInt# (GHC.neInt16# x# y#)
  {-# INLINE (/=#) #-}

-- | @since 1.0.0
instance Eq# Int32# where
  x# ==# y# = Bool.unsafeFromInt# (GHC.eqInt32# x# y#)
  {-# INLINE (==#) #-}

  x# /=# y# = Bool.unsafeFromInt# (GHC.neInt32# x# y#)
  {-# INLINE (/=#) #-}

-- Eq# - Word#, Word8#, Word16#, Word32# ---------------------------------------

-- | @since 1.0.0
instance Eq# Word# where
  x# ==# y# = Bool.unsafeFromInt# (GHC.eqWord# x# y#)
  {-# INLINE (==#) #-}

  x# /=# y# = Bool.unsafeFromInt# (GHC.neWord# x# y#)
  {-# INLINE (/=#) #-}

-- | @since 1.0.0
instance Eq# Word8# where
  x# ==# y# = Bool.unsafeFromInt# (GHC.eqWord8# x# y#)
  {-# INLINE (==#) #-}

  x# /=# y# = Bool.unsafeFromInt# (GHC.neWord8# x# y#)
  {-# INLINE (/=#) #-}

-- | @since 1.0.0
instance Eq# Word16# where
  x# ==# y# = Bool.unsafeFromInt# (GHC.eqWord16# x# y#)
  {-# INLINE (==#) #-}

  x# /=# y# = Bool.unsafeFromInt# (GHC.neWord16# x# y#)
  {-# INLINE (/=#) #-}

-- | @since 1.0.0
instance Eq# Word32# where
  x# ==# y# = Bool.unsafeFromInt# (GHC.eqWord32# x# y#)
  {-# INLINE (==#) #-}

  x# /=# y# = Bool.unsafeFromInt# (GHC.neWord32# x# y#)
  {-# INLINE (/=#) #-}

-- Eq# - Float#, Double# -------------------------------------------------------

-- | @since 1.0.0
instance Eq# Float# where
  x# ==# y# = Bool.unsafeFromInt# (GHC.eqFloat# x# y#)
  {-# INLINE (==#) #-}

  x# /=# y# = Bool.unsafeFromInt# (GHC.neFloat# x# y#)
  {-# INLINE (/=#) #-}

-- | @since 1.0.0
instance Eq# Double# where
  x# ==# y# = Bool.unsafeFromInt# (x# GHC.==## y#)
  {-# INLINE (==#) #-}

  x# /=# y# = Bool.unsafeFromInt# (x# GHC.==## y#)
  {-# INLINE (/=#) #-}

-- Ord# ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
class Ord# (a :: TYPE r) where
  infix 4 `compare#`, >#, >=#, <#, <=#

  -- | TODO
  --
  -- @since 1.0.0
  compare# :: a -> a -> Ordering#

  -- | TODO
  --
  -- @since 1.0.0
  (>#) :: a -> a -> Bool#

  -- | TODO
  --
  -- @since 1.0.0
  (>=#) :: a -> a -> Bool#

  -- | TODO
  --
  -- @since 1.0.0
  (<#) :: a -> a -> Bool#

  -- | TODO
  --
  -- @since 1.0.0
  (<=#) :: a -> a -> Bool#

-- Eq# - Bool# -----------------------------------------------------------------

-- | @since 1.0.0
instance Ord# Addr# where
  compare# x# y# = Ordering# (GHC.gtAddr# x# y# GHC.-# GHC.ltAddr# x# y#)
  {-# INLINE compare# #-}

  x# ># y# = Bool.unsafeFromInt# (GHC.gtAddr# x# y#)
  {-# INLINE (>#) #-}

  x# >=# y# = Bool.unsafeFromInt# (GHC.geAddr# x# y#)
  {-# INLINE (>=#) #-}

  x# <# y# = Bool.unsafeFromInt# (GHC.ltAddr# x# y#)
  {-# INLINE (<#) #-}

  x# <=# y# = Bool.unsafeFromInt# (GHC.leAddr# x# y#)
  {-# INLINE (<=#) #-}

-- Eq# - Bool# -----------------------------------------------------------------

-- | @since 1.0.0
instance Ord# Bool# where
  compare# x# y# = Ordering# (Bool.toInt# (Bool.gt# x# y#) GHC.-# Bool.toInt# (Bool.lt# x# y#))
  {-# INLINE compare# #-}

  x# ># y# = Bool.gt# x# y#
  {-# INLINE (>#) #-}

  x# >=# y# = Bool.ge# x# y#
  {-# INLINE (>=#) #-}

  x# <# y# = Bool.lt# x# y#
  {-# INLINE (<#) #-}

  x# <=# y# = Bool.le# x# y#
  {-# INLINE (<=#) #-}

-- Ord# - Char# ----------------------------------------------------------------

-- | @since 1.0.0
instance Ord# Char# where
  compare# x# y# = Ordering# (GHC.gtChar# x# y# GHC.-# GHC.ltChar# x# y#)
  {-# INLINE compare# #-}

  x# ># y# = Bool.unsafeFromInt# (GHC.gtChar# x# y#)
  {-# INLINE (>#) #-}

  x# >=# y# = Bool.unsafeFromInt# (GHC.geChar# x# y#)
  {-# INLINE (>=#) #-}

  x# <# y# = Bool.unsafeFromInt# (GHC.ltChar# x# y#)
  {-# INLINE (<#) #-}

  x# <=# y# = Bool.unsafeFromInt# (GHC.leChar# x# y#)
  {-# INLINE (<=#) #-}

-- Ord# - Int#, Int8#, Int16#, Int32# ------------------------------------------

-- | @since 1.0.0
instance Ord# Int# where
  compare# x# y# = Ordering# ((x# GHC.># y#) GHC.-# (x# GHC.<# y#))
  {-# INLINE compare# #-}

  x# ># y# = Bool.unsafeFromInt# (x# GHC.># y#)
  {-# INLINE (>#) #-}

  x# >=# y# = Bool.unsafeFromInt# (x# GHC.>=# y#)
  {-# INLINE (>=#) #-}

  x# <# y# = Bool.unsafeFromInt# (x# GHC.<# y#)
  {-# INLINE (<#) #-}

  x# <=# y# = Bool.unsafeFromInt# (x# GHC.<=# y#)
  {-# INLINE (<=#) #-}

-- | @since 1.0.0
instance Ord# Int8# where
  compare# x# y# = Ordering# (GHC.gtInt8# x# y# GHC.-# GHC.ltInt8# x# y#)
  {-# INLINE compare# #-}

  x# ># y# = Bool.unsafeFromInt# (GHC.gtInt8# x# y#)
  {-# INLINE (>#) #-}

  x# >=# y# = Bool.unsafeFromInt# (GHC.geInt8# x# y#)
  {-# INLINE (>=#) #-}

  x# <# y# = Bool.unsafeFromInt# (GHC.ltInt8# x# y#)
  {-# INLINE (<#) #-}

  x# <=# y# = Bool.unsafeFromInt# (GHC.leInt8# x# y#)
  {-# INLINE (<=#) #-}

-- | @since 1.0.0
instance Ord# Int16# where
  compare# x# y# = Ordering# (GHC.gtInt16# x# y# GHC.-# GHC.ltInt16# x# y#)
  {-# INLINE compare# #-}

  x# ># y# = Bool.unsafeFromInt# (GHC.gtInt16# x# y#)
  {-# INLINE (>#) #-}

  x# >=# y# = Bool.unsafeFromInt# (GHC.geInt16# x# y#)
  {-# INLINE (>=#) #-}

  x# <# y# = Bool.unsafeFromInt# (GHC.ltInt16# x# y#)
  {-# INLINE (<#) #-}

  x# <=# y# = Bool.unsafeFromInt# (GHC.leInt16# x# y#)
  {-# INLINE (<=#) #-}

-- | @since 1.0.0
instance Ord# Int32# where
  compare# x# y# = Ordering# (GHC.gtInt32# x# y# GHC.-# GHC.ltInt32# x# y#)
  {-# INLINE compare# #-}

  x# ># y# = Bool.unsafeFromInt# (GHC.gtInt32# x# y#)
  {-# INLINE (>#) #-}

  x# >=# y# = Bool.unsafeFromInt# (GHC.geInt32# x# y#)
  {-# INLINE (>=#) #-}

  x# <# y# = Bool.unsafeFromInt# (GHC.ltInt32# x# y#)
  {-# INLINE (<#) #-}

  x# <=# y# = Bool.unsafeFromInt# (GHC.leInt32# x# y#)
  {-# INLINE (<=#) #-}

-- Ord# - Word#, Word8#, Word16#, Word32# --------------------------------------

-- | @since 1.0.0
instance Ord# Word# where
  compare# x# y# = Ordering# (GHC.gtWord# x# y# GHC.-# GHC.ltWord# x# y#)
  {-# INLINE compare# #-}

  x# ># y# = Bool.unsafeFromInt# (GHC.gtWord# x# y#)
  {-# INLINE (>#) #-}

  x# >=# y# = Bool.unsafeFromInt# (GHC.geWord# x# y#)
  {-# INLINE (>=#) #-}

  x# <# y# = Bool.unsafeFromInt# (GHC.ltWord# x# y#)
  {-# INLINE (<#) #-}

  x# <=# y# = Bool.unsafeFromInt# (GHC.leWord# x# y#)
  {-# INLINE (<=#) #-}

-- | @since 1.0.0
instance Ord# Word8# where
  compare# x# y# = Ordering# (GHC.gtWord8# x# y# GHC.-# GHC.ltWord8# x# y#)
  {-# INLINE compare# #-}

  x# ># y# = Bool.unsafeFromInt# (GHC.gtWord8# x# y#)
  {-# INLINE (>#) #-}

  x# >=# y# = Bool.unsafeFromInt# (GHC.geWord8# x# y#)
  {-# INLINE (>=#) #-}

  x# <# y# = Bool.unsafeFromInt# (GHC.ltWord8# x# y#)
  {-# INLINE (<#) #-}

  x# <=# y# = Bool.unsafeFromInt# (GHC.leWord8# x# y#)
  {-# INLINE (<=#) #-}

-- | @since 1.0.0
instance Ord# Word16# where
  compare# x# y# = Ordering# (GHC.gtWord16# x# y# GHC.-# GHC.ltWord16# x# y#)
  {-# INLINE compare# #-}

  x# ># y# = Bool.unsafeFromInt# (GHC.gtWord16# x# y#)
  {-# INLINE (>#) #-}

  x# >=# y# = Bool.unsafeFromInt# (GHC.geWord16# x# y#)
  {-# INLINE (>=#) #-}

  x# <# y# = Bool.unsafeFromInt# (GHC.ltWord16# x# y#)
  {-# INLINE (<#) #-}

  x# <=# y# = Bool.unsafeFromInt# (GHC.leWord16# x# y#)
  {-# INLINE (<=#) #-}

-- | @since 1.0.0
instance Ord# Word32# where
  compare# x# y# = Ordering# (GHC.gtWord32# x# y# GHC.-# GHC.ltWord32# x# y#)
  {-# INLINE compare# #-}

  x# ># y# = Bool.unsafeFromInt# (GHC.gtWord32# x# y#)
  {-# INLINE (>#) #-}

  x# >=# y# = Bool.unsafeFromInt# (GHC.geWord32# x# y#)
  {-# INLINE (>=#) #-}

  x# <# y# = Bool.unsafeFromInt# (GHC.ltWord32# x# y#)
  {-# INLINE (<#) #-}

  x# <=# y# = Bool.unsafeFromInt# (GHC.leWord32# x# y#)
  {-# INLINE (<=#) #-}
