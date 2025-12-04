import Ast.Base

namespace UniversalIR

inductive BinOp where
  | add | sub | gt
deriving Repr, BEq

inductive Expr where
  | var (name : String)
  | num (n : Nat)
  | binOp (op : BinOp) (left right : Expr)
deriving Repr, BEq

inductive Stmt where
  | assign (var : String) (expr : Expr)
  | if_ (cond : Expr) (then_ else_ : List Stmt)
  | while (cond : Expr) (body : List Stmt)
  | print (expr : Expr)
deriving Repr, BEq

abbrev State := String → Option Nat

-- REAL DENOTATIONAL SEMANTICS ⟦e⟧, ⟦S⟧
notation:50 s " ⟦ " e:51 " ⟧" => denoteExpr e s
notation:50 "⟦ " S:51 " ⟧" => denoteStmt S

def denoteExpr : Expr → State → Nat
  | .var x, s => s x |>.getD 0
  | .num n, _ => n
  | .binOp .add l r, s => ⟦l⟧ s + ⟦r⟧ s
  | .binOp .sub l r, s => ⟦l⟧ s - ⟦r⟧ s
  | .binOp .gt l r, s => if ⟦l⟧ s > ⟦r⟧ s then 1 else 0

def denoteStmt : Stmt → (State → State)
  | .assign x e, s => fun y => if y == x then some (⟦e⟧ s) else s y
  | .print e, s   => s  -- Pure denotation (side effects abstracted)
  | .if_ cond t e, s => 
      if ⟦cond⟧ s ≠ 0 then t.foldl (init := s) (· ∘ ⟦·⟧) else e.foldl (init := s) (· ∘ ⟦·⟧)
  | .while cond body, s => 
      if ⟦cond⟧ s ≠ 0 then ⟦.while cond body⟧ (body.foldl (init := s) (· ∘ ⟦·⟧)) else s

-- Sequential composition (KEY ACADEMIC PROPERTY)
def seq (s1 s2 : State → State) : State → State := s1 ∘ s2

-- EXECUTABLE VERSION (for Main.lean)
def execProgram (stmts : List Stmt) (init : State) : IO Unit := do
  let final := stmts.foldl (init := init) (· ∘ ⟦·⟧)
  IO.println s!"Final state: {final}"

instance : Ast.IR UniversalIR where
  Expr := Expr
  Stmt := Stmt
  State := State
  denoteExpr := denoteExpr
  denoteStmt := denoteStmt
  seq := seq

end UniversalIR
