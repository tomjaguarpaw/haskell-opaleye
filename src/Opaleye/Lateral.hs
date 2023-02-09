-- | You will only need this module if you are using the arrow
-- ('Opaleye.Select.SelectArr') interface to Opaleye.  It is not
-- needed when working only with 'Opaleye.Select.Select's and using
-- the monadic (@do@ notation) interface.

module Opaleye.Lateral
  ( lateral
  , viaLateral
  , laterally
  , bilaterally
  )
where

import Opaleye.Internal.Lateral
