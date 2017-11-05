module Error (
    ProgramError (..)
  ) where


data ProgramError =
  InvalidArgs String
  deriving Eq
