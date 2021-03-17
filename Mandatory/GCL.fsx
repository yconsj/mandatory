//#r "C:/Users/simon/.nuget/packages/fslexyacc.runtime/10.0.0/lib/net46/FsLexYacc.Runtime.dll"
#r "C:/Users/Peetz/.nuget/packages/fslexyacc.runtime/10.0.0/lib/net46/FsLexYacc.Runtime.dll"
open FSharp.Text.Lexing
open System
#load "GCLTypesAST.fs"
open GCLTypesAST
#load "GCLParser.fs"
open GCLParser
#load "GCLLexer.fs"
open GCLLexer

open System.IO;
open System.Text;
//prettyprinter
let rec prettyPrinterC gcl =
        match gcl with
        | AssignExpr(a,e)                           ->  string "(" + a+ ":=" + (prettyPrinterA e) + ")"
        | ArrayAssignExpr(a,e1,e2)                  ->  string "(" + a + "[" + (prettyPrinterA e1) + "]" + ":=" + (prettyPrinterA e2) + ")"
        | SemiColonExpr(e1,e2)                      ->  string "(" + (prettyPrinterC e1) + ";" + (prettyPrinterC e2) + ")"
        | IfExpr(e)                                 ->  string "(if" + (prettyPrinterGC e) + "fi)"
        | DoExpr(e)                                 ->  string "(do" + (prettyPrinterGC e) + "od)"
        | SkipExpr(s)                               ->  string s
and prettyPrinterGC gcl =
    match gcl with
      | GCLoopExpr(e1, e2)                          ->  string "(" + (prettyPrinterGC e1) + "[]" + (prettyPrinterGC e2) + ")"
      | ArrowExpr(b,e)                              ->  string "(" + (prettyPrinterB b) + "->" + (prettyPrinterC e) + ")"
and prettyPrinterB gcl =
    match gcl with
    | Bool(x) -> string x
    | EqualExpr(l,r)                                ->  string "("+(prettyPrinterA l) + "=" + (prettyPrinterA r) + ")"
    | NotEqualExpr(l,r)                             ->  string "("+(prettyPrinterA l) + "!=" + (prettyPrinterA r) + ")"
    | GreaterExpr(l,r)                              ->  string "("+(prettyPrinterA l) + ">" + (prettyPrinterA r) + ")"
    | SmallerExpr(l,r)                              ->  string "("+(prettyPrinterA l) + "<" + (prettyPrinterA r) + ")"
    | EqGreaterExpr(l,r)                            ->  string "("+(prettyPrinterA l) + ">=" + (prettyPrinterA r) + ")"
    | EqSmallerExpr(l,r)                            ->  string "("+(prettyPrinterA l) + "<=" + (prettyPrinterA r) + ")"
    | OrExpr(l,r)                                   ->  string "("+(prettyPrinterB l) + "|" + (prettyPrinterB r) + ")"
    | AndExpr(l,r)                                  ->  string "("+(prettyPrinterB l) + "&" + (prettyPrinterB r) + ")"    
    | CondOrExpr(l,r)                               ->  string "("+(prettyPrinterB l) + "||" + (prettyPrinterB r) + ")"
    | CondAndExpr(l,r)                              ->  string "("+(prettyPrinterB l) + "&&" + (prettyPrinterB r) + ")"    
    | NotExpr(l)                                    ->  string "(!" + (prettyPrinterB l) + ")"
and prettyPrinterA gcl =
    match gcl with
    | Num(x)                                        ->  string x
    | Var(x)                                        ->  string x
    | ArrayExpr(A,x)                                ->  string "(" + A+"[" + (prettyPrinterA x) + "])"
    | UPlusExpr(l)                                  ->  string "("+"" + (prettyPrinterA l) + ")"
    | UMinusExpr(l)                                 ->  string "(-" + (prettyPrinterA l) + ")"
    | TimesExpr(l, r)                               ->  string "("+(prettyPrinterA l) + "*" + (prettyPrinterA r) + ")"
    | DivExpr(l,r)                                  ->  string "("+(prettyPrinterA l) + "/" + (prettyPrinterA r) + ")"
    | PlusExpr(l,r)                                 ->  string "("+(prettyPrinterA l) + "+" + (prettyPrinterA r) + ")"
    | MinusExpr(l,r)                                ->  string "("+(prettyPrinterA l) + "-" + (prettyPrinterA r) + ")"
    | PowExpr(l,r)                                  ->  string "("+(prettyPrinterA l) + "^" + (prettyPrinterA r) + ")"
    | ModExpr(l,r)                                  ->  string "("+(prettyPrinterA l) + "%" + (prettyPrinterA r) + ")"

type Edge =
     | Edge of string
type Node =
     | Node of Edge * string * Edge


let rec compilerC AST n1 n2  count =
        match AST with
        | AssignExpr(a,e)                           ->  [Node(n1, a + ":=" + prettyPrinterA e, n2)]
        | SemiColonExpr(e1,e2)                      ->  let nNew = Edge ("n" + (string (count+1)))
                                                        (compilerC e1 n1 nNew (count+1)) @ (compilerC e2 nNew n2 (count+2))
        | ArrayAssignExpr(a,e1,e2)                  ->  [Node(n1, a + "["+prettyPrinterA e1 + "]:=" + prettyPrinterA e2, n2)]
        | IfExpr(e)                                 ->  compilerGCDET e n1 n2 count ""
        | DoExpr(e)                                 ->  [Node(n1, (computeB e) + "| false)" ,n2)]@(compilerGCDET e n1 n1 count "") //todo fix
        | SkipExpr(s)                               ->  [Node(n1, "skip",n2)]
and compilerGC AST n1 n2 count =
    match AST with
    | ArrowExpr(b,e)                                -> let nNew = Edge ("q" + (string (count+1)))
                                                       [Node(n1, (prettyPrinterB b),nNew)] @ (compilerC e nNew n2 (count+1))
    | GCLoopExpr(e1,e2)                             -> (compilerGC e1 n1 n2 count) @ (compilerGC e2 n1 n2 (count+1))
and compilerGCDET AST n1 n2 count boolx =
    match AST with
    | ArrowExpr(b,e)                                -> let nNew = Edge ("q" + (string (count+1)))
                                                       match boolx with
                                                       | "" -> [Node(n1, (prettyPrinterB b) + "& (!" + boolx + "false)",nNew)] @ (compilerC e nNew n2 (count+1))
                                                       | _  -> [Node(n1, (prettyPrinterB b) + "& (!(" + boolx + "false))",nNew)] @ (compilerC e nNew n2 (count+1))           
    | GCLoopExpr(e1,e2)                             -> (compilerGCDET e1 n1 n2 count boolx) @ (compilerGCDET e2 n1 n2 (count+1) (computeNonDet e1 + "|" + boolx))   
and computeB AST =
    match AST with
    | ArrowExpr(b,e)                                -> "!(" + prettyPrinterB b
    | GCLoopExpr(e1,e2)                             -> "(" + (computeB e1) + ")&("+ (computeB e2) + ")"
and computeNonDet AST =
    match AST with
    | ArrowExpr(b,e)                                -> prettyPrinterB b
    | GCLoopExpr(e1,e2)                             -> "(" + (computeNonDet e1) + ")|("+ (computeNonDet e2) + ")"


// Pase from calculator example, using to parse readline
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = GCLParser.start GCLLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res

let rec graphString list =
    match list with
    | (Node(Edge(n1),action,Edge(n2)))::t           -> string (n1 + "->" + n2 + "[label= \"" + action + "\"]") + graphString t
    | []                                            -> ""
// We implement here the function that interacts with the user
let rec computeAST n =
   if n = 0 then
        printfn "Bye bye"
    else
        printf "Enter a GCL expression: "

        try
            // We parse the input string
            let e = parse (Console.ReadLine())
            // and print the result of evaluating it
            printfn "Result: %s" (prettyPrinterC (e)) //PrettyPrinter
            //TEST do x>0 -> y:=x*y [] x>0 -> y:=x*y od
            printfn "The new set: %O" (compilerC e (Edge("start")) (Edge("slut")) 0)
            
            //Create graph file
            File.WriteAllText("Test.dot", "digraph {" + graphString (compilerC e (Edge("start")) (Edge("slut")) 0) + "}")
            
            
            computeAST 1
        with err -> printfn "Invalid input"
                    computeAST 1

// Start interacting with the user
computeAST 1