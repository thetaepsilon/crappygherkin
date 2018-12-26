module User.Ds2.CrappyGherkin.LineParse where

import User.Ds2.CrappyGherkin.Types

-- returns Just remainder on match,
-- where remainder is the list tail after the prefix match;
-- otherwise Nothing is returned.
matchesPrefix :: Eq a => [a] -> [a] -> Maybe [a]
matchesPrefix prefix input =
  case prefix of
    [] -> Just input
    (p:ps) ->
      case input of
        (i:is) -> if (p == i)
          then matchesPrefix ps is
          else Nothing
        [] -> Nothing



-- try to figure out what this line is looking at it's beginning.
-- note that things like the typical colon after e.g.
-- Feature: is not checked here; that's done later.
-- this enables us to give the user hints if they forget that.
-- as a linear search is used to locate matches,
-- the table is organised in approximate order of frequency of keyword.
guesses_table :: [(Char, String, GherkinTag)]
guesses_table = [
  ('A', "nd", And),
  ('T', "hen", Then),
  ('W', "hen", When),
  ('G', "iven", Given),
  ('|', "", PipeExampleLine),
  ('E', "xamples", Examples),
  ('S', "cenario", Scenario),
  ('F', "eature", Feature)]

findFirstMaybe :: (a -> Bool) -> [a] -> Maybe a
findFirstMaybe _ [] = Nothing
findFirstMaybe f (a:as) =
  if (f a)
    then Just a
    else findFirstMaybe f as

-- argh, why don't tuples compose or break down elegantly...
fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a



guessTag :: String -> Maybe (GherkinTag, String)
guessTag (l:ine) = do
  (char, prefix, tag) <- findFirstMaybe ((l ==) . fst3) guesses_table
  -- fails in case of no match, which means our guess is incorrect;
  -- the short circuit behaviour therefore does what we want here.
  restOfLine <- matchesPrefix prefix ine
  return (tag, restOfLine)

