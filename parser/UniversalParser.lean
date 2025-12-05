import Parser
import Ast.UniversalIR

namespace UniversalParser

open Parser

-- ID: letter followed by alphanumeric
def pID : Parser String := letter *> (alpha <|> digit)* |>.map String.join

-- Number → Expr
def pNumber : Parser UniversalIR.Expr := nat.map UniversalIR.Expr.num

-- Atoms: number | id | (expr)
def pAtom : Parser UniversalIR.Expr := choice [
  pNumber,
  pID.map UniversalIR.Expr.var,
  symbol "(" *> pExpr <* symbol ")"
]

-- Binary operators (left-associative): + - >
def pExpr : Parser UniversalIR.Expr := do
  let mut left ← pAtom
  while true do
    if (← test "+") then
      let right ← pAtom
      left := UniversalIR.Expr.binOp UniversalIR.BinOp.add left right
    else if (← test "-") then
      let right ← pAtom
      left := UniversalIR.Expr.binOp UniversalIR.BinOp.sub left right
    else if (← test ">") then
      let right ← pAtom
      left := UniversalIR.Expr.binOp UniversalIR.BinOp.gt left right
    else
      break left
  pure left

-- Statements
def pStatement : Parser UniversalIR.Stmt := choice [
  -- set x := expr ;
  do
    keyword "set"
    let x ← pID
    symbol ":="
    let e ← pExpr
    symbol ";"
    pure (UniversalIR.Stmt.set x e),
  
  -- print expr ;
  do
    keyword "print"
    let e ← pExpr
    symbol ";"
    pure (UniversalIR.Stmt.print e),
  
  -- while expr do stmt* end
  do
    keyword "while"
    let cond ← pExpr
    keyword "do"
    let body ← many pStatement
    keyword "end"
    pure (UniversalIR.Stmt.while cond body)
]

-- Full program
def pProgram : Parser (List UniversalIR.Stmt) := many pStatement

-- Parse entry point
def parseProgram (input : String) : Except String (List UniversalIR.Stmt) := 
  runParser pProgram input "" |>.toExcept toString

end UniversalParser
