module User.Ds2.CrappyGherkin.Types where

data GherkinTag
  = Feature | Scenario
  | Given | When | Then | And
  | Examples | PipeExampleLine
    deriving Show

