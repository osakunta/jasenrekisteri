{-# LANGUAGE RankNTypes #-}
module Control.Lens.Att where

import Control.Lens
import Futurice.Prelude
import Prelude ()

-- | 'Att' provides a 'Lens' that can be used to read,
-- or write the value associated with a key in a function-like
-- container on an ad hoc basis.
--
-- An instance of 'Att' should satisfy:
--
-- @
-- 'ix' k ≡ 'att' k
-- @
class Ixed m => Att m where
    att :: Index m -> Lens' m (IxValue m)

instance Eq e => Att (e -> a) where
    att e p f = p (f e) <&> \a e' -> if e == e' then a else f e'
    {-# INLINE att #-}
