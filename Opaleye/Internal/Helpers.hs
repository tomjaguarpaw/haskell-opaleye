module Opaleye.Internal.Helpers where

infixr 8 .:

(.:) :: (r -> z) -> (a -> b -> r) -> a -> b -> z
(.:) f g x y = f (g x y)

infixr 8 .:.

(.:.) :: (r -> z) -> (a -> b -> c -> r) -> a -> b -> c -> z
(.:.) f g a b c = f (g a b c)
