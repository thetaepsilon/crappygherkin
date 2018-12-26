module User.Ds2.AST.Recursive (
  Tree, Folder, Validator, ASTError, ASTValidated,
  buildFromTree, buildFromTreeEither
) where

import User.Ds2.DoFold
import Data.Either.Combinators (mapLeft)

-- Construct a validated object representation from a tree of elements.
-- each element first has it's children recursively validated depth first,
-- before the validated elements are folded into a "state" object.
-- At the end, a function is then invoked on the state object,
-- returning a final representation object.

-- By using appropriate types that can capture an error state,
-- the "combine" and "validate" stages could e.g. return a Left someErr.



class Functor t => Tree t where
  treeNode :: t a -> a
  treeChildren :: t a -> [t a]

data SimpleTree a = SimpleTree { _node :: a, _children :: [SimpleTree a] }
  deriving Show
toNode x = SimpleTree x []

instance Functor SimpleTree where
  fmap f t =
    let
      node = f (treeNode t)
      children = map (fmap f) (treeChildren t)
    in SimpleTree node children

instance Tree SimpleTree where
  treeNode = _node
  treeChildren = _children

simpleTreeNode :: a -> SimpleTree a
simpleTreeNode a = SimpleTree a []





type Folder state validated = state -> [validated] -> state
type Validator state validated = state -> validated

buildFromTree :: Tree t => Folder u v -> Validator u v -> t u -> v
buildFromTree fold validate tree =
  let
    go = (\t -> validate (fold (treeNode t) (fmap go (treeChildren t))))
  in go tree



-- build-up process operating using Either.
-- the folding function (not given a list this time)
-- is only ever applied to state and validated elements
-- that did not return a Left during their processing.
-- type variables:
-- v: validated node
-- st: state during build-up of a node
-- eac: add child error.
--	occurs when a fold step of a child element fails.
--	this could mean e.g. the parent could not accept a child element of that kind.
-- esv: state validation error
--	if a state object is not ready to become validated.
--	this could be because of e.g. child elements being missing or otherwise lacking info,
--	even if the children are valid in themselves or it was not an immediate error to add that child.
-- esi: state initialiation error.
--	this is not created by the function itself,
--	but rather is present from the tree from e.g. parsing stages.
-- NB: error results only return the offending data structure as appropriate,
-- there is no tree path tracking going on here.
-- it is intended that if this is desired,
-- the input tree nodes should include information on their origin line numbers etc.
data ASTError st child eac esv esi =
  AddChildError eac st child
  | StateValidateError esv st
  | StateInitError esi

-- for this tree, our state type is an Either;
-- either the above AST error or the "real" state type.
-- the child type has to make an appearance here due to it being present in error values
-- (this is so the offending tree node can be returned).
type ASTState st child eac esv esi = Either (ASTError st child eac esv esi) st

-- the "validated" type also retains the possibility of being an error,
-- if the input state to validation was itself an error.
type ASTValidated st child eac esv esi = Either (ASTError st child eac esv esi) child

-- hence, our validator will involve passing through Left's of the Either,
-- and calling the actual validator in the event of a Right (e.g. check for incomplete children).
astEitherValidator ::
  (st -> Either esv child)
  -> Validator (ASTState st child eac esv esi) (ASTValidated st child eac esv esi)

-- some glue to promote the inner validator's failure to ASTError...
-- this way we can then perform validation inside matched Either monads.
promoteInnerValidator ::
  (st -> Either esv child)
  -> st
  -> Either (ASTError st child eac esv esi) child

promoteInnerValidator f st =
  let adapter = flip StateValidateError st
  in (mapLeft adapter) $ f st

astEitherValidator innerV _st =
  let promoted = promoteInnerValidator innerV
  in _st >>= promoted


-- the folder then just becomes a monadic Either fold.
-- preferably one that short circuits early when a Left is encountered.
-- the passed fold combination function here is AST domain logic;
-- it determines whether a given child may be added to the current node being processed.
-- it either returns an error describing why this could not happen, or the combined node.
astEitherFolder ::
  (st -> child -> Either eac st)
  -> Folder (ASTState st child eac esv esi) (ASTValidated st child eac esv esi)

-- some more intelligent Either promotion...
promoteInnerFolder ::
  (st -> child -> Either eac st)
  -> st -> child -> ASTState st child eac esv esi
promoteInnerFolder f st child =
  -- hmm, I'm not aware of a flip variant I could use for this 3-arg case
  let adapter = \e -> AddChildError e st child
  in mapLeft adapter (f st child)

astEitherFolder innerF = doFold (promoteInnerFolder innerF)





-- now, to tie it all together.
-- we take as input a tree of objects which either ran into an initialisation error
-- (e.g. some data about the node *itself* turned out to be invalid, like a name),
-- or an initial state to kick off folding.
-- so we take care of that as well.
buildFromTreeEither :: Tree t =>
  (st -> child -> Either eac st)	-- child-node combine function
  -> (st -> Either esv child)		-- validation of "finished" nodes
  -> t (Either esi st)			-- tree of init states
  -> ASTValidated st child eac esv esi	-- combined tree

buildFromTreeEither combine check input =
  let
    mapped = fmap (mapLeft StateInitError) input	-- adapt to correct type
    folder = astEitherFolder combine
    validator = astEitherValidator check
  in buildFromTree folder validator mapped




