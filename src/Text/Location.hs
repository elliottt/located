{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module Text.Location where

import           Data.Int (Int64)
import qualified Data.Text.Lazy as L
import           GHC.Generics (Generic)

#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup(..))
#endif


data Position = Position { posSource :: !String
                         , posIndex
                         , posRow
                         , posCol :: !Int64
                         } deriving (Show,Eq,Ord,Generic)

data Range = Range { rangeStart
                   , rangeEnd :: !Position
                   } deriving (Show,Eq,Ord,Generic)


data Located a =
  Located { locRange :: !Range
          , locValue :: a
          } deriving (Functor,Foldable,Traversable,Show,Generic)


-- | Remove one layer of location information.
class UnLoc a where
  unLoc :: a -> a

-- We can't remove location information from a located thing, but we can remove
-- location information from the inner thing.
instance UnLoc a => UnLoc (Located a) where
  unLoc = fmap unLoc

instance UnLoc a => UnLoc [a] where
  unLoc = fmap unLoc

instance UnLoc a => UnLoc (Maybe a) where
  unLoc = fmap unLoc


{-# SPECIALIZE (<->) :: Range -> Range -> Range #-}
(<->) :: (HasRange a, HasRange b) => a -> b -> Range
left <-> right = Range { rangeStart = rangeStart (getRange left)
                       , rangeEnd   = rangeEnd (getRange right) }

class HasRange a where
  getRange :: a -> Range

instance HasRange Range where
  getRange = id
  {-# INLINE getRange #-}

instance HasRange (Located a) where
  getRange = locRange
  {-# INLINE getRange #-}

instance (HasRange a, HasRange b) => HasRange (Either a b) where
  getRange (Left  a) = getRange a
  getRange (Right b) = getRange b

instance (HasRange a, HasRange b) => HasRange (a,b) where
  getRange (a,b) = a <-> b


at :: HasRange loc => a -> loc -> Located a
at locValue loc = Located { locRange = getRange loc, .. }

thing :: Located a -> a
thing  = locValue


-- | Move a position by the width of a character.
movePos :: Int64 -- ^ Tab size
        -> Char -> Position -> Position
movePos tabSize = go
  where
  go '\t' p = p { posCol = posCol p + tabSize }
  go '\n' p = p { posRow = posRow p + 1, posCol = 1 }
  go '\r' p = p
  go _    p = p { posCol = posCol p + 1 }


inRange :: Range -> Position -> Bool
inRange Range { .. } = \ pos -> rangeStart <= pos && pos <= rangeEnd
{-# INLINE inRange #-}


zeroPos :: String -> Position
zeroPos src = Position { posSource = src, posIndex = 0, posRow = 1, posCol = 1 }


-- | The lines that the region describes, with optional additional lines of
-- context.
rangeText :: Int -> Range -> L.Text -> L.Text
rangeText cxt Range { .. } txt = L.unlines
                               $ take len
                               $ drop start
                               $ L.lines txt
  where
  start = max 0 (fromIntegral (posRow rangeStart) - cxt - 1)
  len   = max 1 (cxt + fromIntegral (posRow rangeEnd - posRow rangeStart) + 1)

#if MIN_VERSION_base(4,9,0)
instance Semigroup Range where
  Range l1 r1 <> Range l2 r2 | l1 > r2   = Range l2 r1
                             | otherwise = Range l1 r2

  stimes _ r = r
  {-# INLINE stimes #-}
#endif
