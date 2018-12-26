module User.Ds2.DoFold where

-- monadic version of foldl using do-notation.
-- yes, I know this already exists as foldM,
-- and that this doesn't currently support arbitary Foldable types...
doFold :: Monad m => (st -> e -> m st) -> m st -> [m e] -> m st
doFold f _state [] = _state
doFold f _state (_e:es) = do
  state <- _state
  elem <- _e
  let _next = f state elem
  doFold f _next es



