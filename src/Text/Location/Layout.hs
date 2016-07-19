{-# LANGUAGE RecordWildCards #-}

module Text.Location.Layout (
    Layout(..),
    layout,
  ) where

import Text.Location


data Layout a = Layout { beginsLayout :: a -> Bool
                         -- ^ True when this token begins layout

                       , endsLayout :: a -> Bool
                         -- ^ True when this token explicitly ends layout

                       , sep :: a
                         -- ^ The separator token

                       , start :: a
                         -- ^ Layout block starting token

                       , end :: a
                         -- ^ Layout block ending token
                       }

layout :: Layout a -> [Located source a] -> [Located source a]
layout Layout { .. } = go Nothing []
  where
  startCol Range { rangeStart = Position { .. } } = posCol

  currentLevel (loc : _) = startCol loc
  currentLevel []        = 0

  -- a new layout level has been started, emit a starting token, and push the
  -- current level on the stack.
  go Just{} stack (tok@Located { .. } : toks) =
    (start `at` locRange) : tok : go Nothing (locRange:stack) toks

  go (Just loc) stack [] =
    (start `at` loc) : go Nothing (loc : stack) []

  go Nothing stack ts@(tok@Located { .. } : toks)

    -- when the next token would close the current level
    | startCol locRange < currentLevel stack =
      (end `at` locRange) : go Nothing (tail stack) ts

    | beginsLayout locValue =
      let sepToks | startCol locRange == currentLevel stack = [sep `at` locRange]
                  | otherwise                               = []
       in sepToks ++ tok : go (Just locRange) stack toks

    | endsLayout locValue =
      (end `at` locRange) : tok : go Nothing (tail stack) toks

    | startCol locRange == currentLevel stack =
      (sep `at` locRange) : tok : go Nothing stack toks

    | otherwise =
      tok : go Nothing stack toks

  go _ stack [] =
    [ end `at` loc | loc <- stack ]
