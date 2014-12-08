module Opaleye.Internal.Tag where

data Tag = UnsafeTag Int deriving (Read, Show)

start :: Tag
start = UnsafeTag 1

next :: Tag -> Tag
next = UnsafeTag . (+1) . unsafeUnTag

unsafeUnTag :: Tag -> Int
unsafeUnTag (UnsafeTag i) = i

tagWith :: Tag -> String -> String
tagWith t = appendShow (unsafeUnTag t) . (++ "_")

appendShow :: Show a => a -> String -> String
appendShow = flip (++) . show
