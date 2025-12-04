# Practical Pseudocode Compiler in Lean 4

This project implements a student-friendly pseudocode compiler using Lean 4's type system for verified semantics. Parses clean pseudocode → typed AST → denotational semantics → verified Lean/C/JS codegen. Designed for classroom use with future dialect expansion.


## To build and run:
# lake build
# lake exe pseudocode_compiler test.pseudo

## test.pseudo
set x := 5;
while x > 0 do
  print x;
  set x := x - 1;
end

## Grammar Specification (Phase 1)
Program    ::= Statement*
Statement  ::= "set" ID ":=" Expr ";" 
             | "if" Expr "then" Statement* ["else" Statement*] "end" 
             | "while" Expr "do" Statement* "end"
             | "print" Expr ";"
Expr       ::= ID | Number | "(" Expr ")" | Expr ("+"|"-") Expr
ID         ::= [a-zA-Z_][a-zA-Z0-9_]*

This pseudocompiler will support multiple grammar implementations inspried by the idea of a universal grammar: https://en.wikipedia.org/wiki/Applicative_universal_grammar

## Architecture
Pseudocode → Parser (lean4-parser) → Typed AST → Semantics → Codegen (Lean/C/JS)

## Denotational Semantics
Formal semantics defined as state transformers: ⟦S⟧ : State → State where State = Name → Option Nat

inductive Expr where
  | var (name : String) | num (n : Nat) | binOp (op : BinOp) (l r : Expr)

inductive Stmt where
  | assign (x : String) (e : Expr)
  | if_ (cond : Expr) (then_ else_ : List Stmt)
  | while (cond : Expr) (body : List Stmt)
  | print (e : Expr)

def evalExpr : Expr → State → Nat
def ⟦assign x e⟧ s := update x (evalExpr e s) s
def ⟦while c body⟧ s := if evalExpr c s ≠ 0 then ⟦body⟧ (⟦while c body⟧ s) else s

## Implementation

Parser (using lean4-parser combinators):
def pStatement : Parser Stmt :=
  ("set" *> pID <* ":=" <*> pExpr <* ";").map (λ⟨x,e⟩ => Stmt.assign x e)
  <|> ("if" *> pExpr <* "then" <*> many pStatement <*> 
       optional ("else" *> many pStatement) <* "end").map IfThenElse

Setup:
lake init pseudocode_compiler

lakefile.toml:
[[require]]
name = "Parser"
git = "https://github.com/fgdorais/lean4-parser"

## 6-Week Development Plan
1. Weeks 1-2: Parser + AST + roundtrip tests
2. Week 3: Denotational interpreter + type checker  
3. Week 4: Error recovery + symbol tables
4. Week 5: Lean 4 codegen (elaboration)
5. Week 6: C/JS backends + test suite

## Example
set x := 5;
while x > 0 do
  print x;
  set x := x - 1;
end
Expected: Prints 5,4,3,2,1 with verified semantics.

## Academic Contributions
1. Verified Semantics: Lean 4 proves semantic preservation
2. Clean Grammar: LL(1) parsing suitable for pedagogy  
3. Extensible Design: Dialect registry for future work
4. Practical Scope: 90% student pseudocode coverage

Future Work: Multi-grammar dispatch, optimization passes, Jupyter integration.
Single maintainer implementable in 6 weeks. Focuses on Lean's verification strengths.
