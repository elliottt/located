{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}

module Text.Location where

import           Control.Applicative ((<|>))
import           Data.Function (on)
import           Data.Int (Int64)
import qualified Data.Text.Lazy as L
import           GHC.Generics


data Position = Position { posRow, posCol :: !Int64
                         } deriving (Show,Eq,Ord,Generic)

data Range source = Range { rangeSource :: Maybe source
                          , rangeStart, rangeEnd :: !Position
                          } deriving (Show,Eq,Ord,Generic)

data Located source a =
  Located { locRange :: !(Range source)
          , locValue :: a
          } deriving (Functor,Foldable,Traversable,Show,Generic)

instance Eq source => Eq (Located source a) where
  (==) = (==) `on` locRange
  (/=) = (/=) `on` locRange
  {-# INLINE (==) #-}
  {-# INLINE (/=) #-}

instance Ord source => Ord (Located source a) where
  compare = compare `on` locRange
  {-# INLINE compare #-}


-- | Remove one layer of location information.
class UnLoc a where
  unLoc :: a -> a

-- We can't remove location information from a located thing, but we can remove
-- location information from the inner thing.
instance UnLoc a => UnLoc (Located source a) where
  unLoc = fmap unLoc

instance UnLoc a => UnLoc [a] where
  unLoc = fmap unLoc

instance UnLoc a => UnLoc (Maybe a) where
  unLoc = fmap unLoc


class HasLoc a where
  type LocSource a :: *
  getLoc :: a -> Range (LocSource a)

instance HasLoc a => HasLoc (Maybe a) where
  type LocSource (Maybe a) = LocSource a
  getLoc = foldMap getLoc

instance HasLoc a => HasLoc [a] where
  type LocSource [a] = LocSource a
  getLoc = foldMap getLoc

instance (LocSource a ~ LocSource b, HasLoc a, HasLoc b) => HasLoc (a,b) where
  type LocSource (a,b) = LocSource a
  getLoc (a,b) = mappend (getLoc a) (getLoc b)

instance ( LocSource a ~ LocSource b, LocSource b ~ LocSource c, HasLoc a
         , HasLoc b, HasLoc c) => HasLoc (a,b,c) where
  type LocSource (a,b,c) = LocSource a
  getLoc (a,b,c) = mconcat [ getLoc a, getLoc b, getLoc c ]

instance ( LocSource a ~ LocSource b, LocSource b ~ LocSource c
         , LocSource c ~ LocSource d , HasLoc a, HasLoc b, HasLoc c
         , HasLoc d) => HasLoc (a,b,c,d) where
  type LocSource (a,b,c,d) = LocSource a
  getLoc (a,b,c,d) = mconcat [ getLoc a, getLoc b, getLoc c, getLoc d ]

instance HasLoc (Range source) where
  type LocSource (Range source) = source
  getLoc = id

instance HasLoc (Located source a) where
  type LocSource (Located source a) = source
  getLoc = locRange
  {-# INLINE getLoc #-}

at :: HasLoc loc => a -> loc -> Located (LocSource loc) a
at locValue loc = Located { locRange = getLoc loc, .. }

thing :: Located source a -> a
thing Located { .. } = locValue


-- | Move a position by the width of a character.
movePos :: Int64 -- ^ Tab size
        -> Char -> Position -> Position
movePos tabSize = \ c p ->
  if | c == '\t' -> p { posCol = posCol p + tabSize }
     | c == '\n' -> p { posRow = posRow p + 1, posCol = 1 }
     | c == '\r' -> p
     | otherwise -> p { posCol = posCol p + 1 }


inRange :: Range source -> Position -> Bool
inRange Range { .. } = \ pos -> rangeStart <= pos && pos <= rangeEnd
{-# INLINE inRange #-}


zeroPos :: Position
zeroPos  = Position { posRow = 1, posCol = 1 }


-- | The lines that the region describes, with optional additional lines of
-- context.
rangeText :: Int -> Range source -> L.Text -> L.Text
rangeText cxt Range { .. } txt = L.unlines
                               $ take len
                               $ drop start
                               $ L.lines txt
  where
  start = max 0 (fromIntegral (posRow rangeStart) - cxt - 1)
  len   = max 1 (cxt + fromIntegral (posRow rangeEnd - posRow rangeStart) + 1)

instance Monoid (Range source) where
  mempty = Range { rangeSource = Nothing
                 , rangeStart  = zeroPos
                 , rangeEnd    = zeroPos }

  mappend (Range s1 l1 r1) (Range s2 l2 r2)
    | l1 > r2   = Range (s1 <|> s2) l2 r1
    | otherwise = Range (s1 <|> s2) l1 r2
