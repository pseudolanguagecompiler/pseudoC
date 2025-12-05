-- WORK IN PROGRESS: Simplified Python grammar (INCOMPLETE)
-- Missing: full function defs, classes, imports, comprehensions, error handling

inductive PyBinOp where
  | add | sub | mul | div | eq | lt | gt
deriving Repr, BEq

inductive PyExpr where
  | var (name
