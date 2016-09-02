{-# LANGUAGE RecordWildCards #-}

module Text.Location.Layout (
    Layout(..),
    layout,
  ) where

import Text.Location


data Layout tok = Layout { beginsLayout :: tok -> Bool
                           -- ^ True when this token begins layout
  
                         , endsLayout :: tok -> Bool
                           -- ^ True when this token explicitly ends layout
  
                         , sep :: Range -> tok
                           -- ^ The separator token
  
                         , start :: Range -> tok
                           -- ^ Layout block starting token
  
                         , end :: Range -> tok
                           -- ^ Layout block ending token
                         }

layout :: HasRange tok => Layout tok -> [tok] -> [tok]
layout Layout { .. } = go Nothing []
  where
  startCol Range { rangeStart = Position { .. } } = posCol

  currentLevel (loc : _) = startCol loc
  currentLevel []        = 0

  -- a new layout level has been started, emit a starting token, and push the
  -- current level on the stack.
  go Just{} stack (tok : toks) =
    let loc = getRange tok
     in start loc : tok : go Nothing (loc:stack) toks

  go (Just loc) stack [] =
    start loc : go Nothing (loc : stack) []

  go Nothing stack ts@(tok : toks)

    -- when the next token would close the current level
    | startCol loc < currentLevel stack =
      end loc : go Nothing (tail stack) ts

    | beginsLayout tok =
      let sepToks | startCol loc == currentLevel stack = [sep loc]
                  | otherwise                          = []
       in sepToks ++ tok : go (Just loc) stack toks

    | endsLayout tok =
      end loc : tok : go Nothing (tail stack) toks

    | startCol loc == currentLevel stack =
      sep loc : tok : go Nothing stack toks

    | otherwise =
      tok : go Nothing stack toks

    where
    loc = getRange loc

  go _ stack [] =
    [ end loc | loc <- stack ]
