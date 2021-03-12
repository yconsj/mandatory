module GCLTypesAST

//Different types of expressions
type arithmeticExpr =
  | Num of float
  | Var of string
  | TimesExpr of (arithmeticExpr * arithmeticExpr)
  | DivExpr of (arithmeticExpr * arithmeticExpr)
  | PlusExpr of (arithmeticExpr *arithmeticExpr)
  | MinusExpr of (arithmeticExpr * arithmeticExpr)
  | PowExpr of (arithmeticExpr * arithmeticExpr)
  | ModExpr of (arithmeticExpr * arithmeticExpr)
  | UPlusExpr of (arithmeticExpr)
  | UMinusExpr of (arithmeticExpr)
  | ArrayExpr of (String*arithmeticExpr)

 type boolExpr = 
  | Bool of bool
  | CondOrExpr of (boolExpr * boolExpr)
  | CondAndExpr of (boolExpr * boolExpr)
  | OrExpr of (boolExpr * boolExpr)
  | AndExpr of (boolExpr * boolExpr)
  | NotExpr of (boolExpr)
  | EqualExpr of (arithmeticExpr * arithmeticExpr)
  | NotEqualExpr of (arithmeticExpr * arithmeticExpr)
  | GreaterExpr of (arithmeticExpr * arithmeticExpr)
  | SmallerExpr of (arithmeticExpr * arithmeticExpr)
  | EqGreaterExpr of (arithmeticExpr * arithmeticExpr)
  | EqSmallerExpr of (arithmeticExpr * arithmeticExpr)

type CExpr =
  | AssignExpr of (string * arithmeticExpr)
  | ArrayAssignExpr of (string * arithmeticExpr * arithmeticExpr)
  | SemiColonExpr of (CExpr * CExpr)
  | IfExpr of GCExpr
  | DoExpr of GCExpr
  | SkipExpr of string

and GCExpr =
  | ArrowExpr of (boolExpr * CExpr)
  | GCLoopExpr of (GCExpr * GCExpr)
