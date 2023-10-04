module Opaleye.Internal.Tag where

import Control.Monad.Trans.State.Strict ( get, modify', State )

-- | Tag is for use as a source of unique IDs in QueryArr
newtype Tag = UnsafeTag Int deriving (Eq, Ord, Read, Show)

start :: Tag
start = UnsafeTag 1

next :: Tag -> Tag
next = UnsafeTag . (+1) . unsafeUnTag

unsafeUnTag :: Tag -> Int
unsafeUnTag (UnsafeTag i) = i

tagWith :: Tag -> String -> String
tagWith t s = s ++ "_" ++ show (unsafeUnTag t)

fresh :: State Tag Tag
fresh = do
  t <- get
  modify' next
  pure t
