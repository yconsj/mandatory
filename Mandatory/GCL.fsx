#r "C:/Users/simon/.nuget/packages/fslexyacc.runtime/10.0.0/lib/net46/FsLexYacc.Runtime.dll"
open FSharp.Text.Lexing
open System
#load "GCLTypesAST.fs"
open GCLTypesAST
#load "GCLParser.fs"
open GCLParser
#load "GCLLexer.fs"
open GCLLexer


let rec prettyPrinterB gcl =
        match gcl with
        | Bool(x) -> string x
        | EqualExpr(l,r)      ->  string "("+(prettyPrinterA l) + "=" + (prettyPrinterA r) + ")"
        | NotEqualExpr(l,r)   ->  string "("+(prettyPrinterA l) + "!=" + (prettyPrinterA r) + ")"
        | GreaterExpr(l,r)   ->  string "("+(prettyPrinterA l) + ">" + (prettyPrinterA r) + ")"
        | SmallerExpr(l,r)   ->  string "("+(prettyPrinterA l) + "<" + (prettyPrinterA r) + ")"
        | EqGreaterExpr(l,r)   ->  string "("+(prettyPrinterA l) + ">=" + (prettyPrinterA r) + ")"
        | EqSmallerExpr(l,r)   ->  string "("+(prettyPrinterA l) + "<=" + (prettyPrinterA r) + ")"
        | OrExpr(l,r)       ->  string "("+(prettyPrinterB l) + "||" + (prettyPrinterB r) + ")"
        | AndExpr(l,r)       ->  string "("+(prettyPrinterB l) + "&&" + (prettyPrinterB r) + ")"    
        | NotExpr(l)       ->  string "(!" + (prettyPrinterB l) + ")"
and prettyPrinterA gcl =
    match gcl with
    | Num(x) ->  string x
    | Var(x) -> string x
    | ArrayExpr(A,x) -> string "(" + A+"[" + (prettyPrinterA x) + "])"
    | UPlusExpr(l)      ->  string "("+"" + (prettyPrinterA l) + ")"
    | UMinusExpr(l)     ->  string "(-" + (prettyPrinterA l) + ")"
    | TimesExpr(l, r)    ->  string  "("+(prettyPrinterA l) + "*" + (prettyPrinterA r) + ")"
    | DivExpr(l,r)        ->  string "("+(prettyPrinterA l) + "/" + (prettyPrinterA r) + ")"
    | PlusExpr(l,r)       ->  string "("+(prettyPrinterA l) + "+" + (prettyPrinterA r) + ")"
    | MinusExpr(l,r)      ->  string "("+(prettyPrinterA l) + "-" + (prettyPrinterA r) + ")"
    | PowExpr(l,r)        ->  string "("+(prettyPrinterA l) + "^" + (prettyPrinterA r) + ")"
    | ModExpr(l,r)        ->  string "("+(prettyPrinterA l) + "%" + (prettyPrinterA r) + ")"
// We
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = GCLParser.start GCLLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res

// We implement here the function that interacts with the user
let rec compute n =
    if n = 0 then
        printfn "Bye bye"
    else
        printf "Enter an GCL: "
        try
        // We parse the input string
        let gcl = parse (Console.ReadLine())
        // and print the result of evaluating it
        printfn "Result: %s" (prettyPrinterB gcl)
        //printfn "Result: %A" (gcl)
        compute n
        with err -> compute (n-1)

// Start interacting with the user
compute 3