#r "C:/Users/simon/.nuget/packages/fslexyacc.runtime/10.0.0/lib/net46/FsLexYacc.Runtime.dll"
//#r "C:/Users/Peetz/.nuget/packages/fslexyacc.runtime/10.0.0/lib/net46/FsLexYacc.Runtime.dll"
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



let rec compilerCNonDet AST n1 n2 count =
        match AST with
        | AssignExpr(a,e)                           ->  [Edge(n1, AssignAction(a, e), n2)]
        | SemiColonExpr(e1,e2)                      ->  let nNew = Node ("n" + (string (count+1)))
                                                        (compilerCNonDet e1 n1 nNew (count+1)) @ (compilerCNonDet e2 nNew n2 (count+2))
        | ArrayAssignExpr(a,e1,e2)                  ->  [Edge(n1, ArrayAssignAction(a,e1,e2), n2)]
        | IfExpr(e)                                 ->  compilerGC e n1 n2 count
        | DoExpr(e)                                 ->  [Edge(n1, BooleanAction(NotExpr(computeB e)) ,n2)]@(compilerGC e n1 n1 count)
        | SkipExpr(s)                               ->  [Edge(n1, SkipAction,n2)]
and compilerCDET AST n1 n2 count =
        match AST with
        | AssignExpr(a,e)                           ->  [Edge(n1, AssignAction(a, e), n2)]
        | SemiColonExpr(e1,e2)                      ->  let nNew = Node ("n" + (string (count+1)))
                                                        (compilerCDET e1 n1 nNew (count+1)) @ (compilerCDET e2 nNew n2 (count+2))
        | ArrayAssignExpr(a,e1,e2)                  ->  [Edge(n1, ArrayAssignAction(a,e1,e2), n2)]
        | IfExpr(e)                                 ->  let (b, enew) = compilerGCDET e n1 n2 count (Bool(false))
                                                        enew
        | DoExpr(e)                                 ->  let (b,enew) = (compilerGCDET e n1 n1 count (Bool(false)))
                                                        [Edge(n1, BooleanAction(NotExpr((computeB e))) ,n2)]@enew
        | SkipExpr(s)                               ->  [Edge(n1, SkipAction,n2)]
and compilerGC AST n1 n2 count =
    match AST with
    | ArrowExpr(b,e)                                -> let nNew = Node ("q" + (string (count+1)))
                                                       [Edge(n1, BooleanAction b,nNew)] @ (compilerCNonDet e nNew n2 (count+1))
    | GCLoopExpr(e1,e2)                             -> (compilerGC e1 n1 n2 count) @ (compilerGC e2 n1 n2 (count+1))
and compilerGCDET AST n1 n2 count boolx =
    match AST with
    | ArrowExpr(b,e)                                -> let nNew = Node ("q" + (string (count+1)))
                                                       let elist = ([Edge(n1, BooleanAction(AndExpr(b, NotExpr(boolx))),nNew)] @ (compilerCDET e nNew n2 (count+1)))
                                                       ((OrExpr (b, boolx)), elist)       
    | GCLoopExpr(e1,e2)                             -> let (b,e) = (compilerGCDET e1 n1 n2 count boolx)
                                                       let (bnew, enew) = (compilerGCDET e2 n1 n2 (count+1) b)
                                                       (bnew,enew@e)
and computeB AST =
    match AST with
    | ArrowExpr(b,e)                                -> b
    | GCLoopExpr(e1,e2)                             -> OrExpr((computeB e1), (computeB e2))


// Interpter -------------
//hvordan looper man?
//forskel på | og ||?
// arrays?
let rec loop pg mem curr =  let (x, newMem) = findNode pg mem curr
                            match x with
                            | (Node("slut")) -> mem 
                            | _ -> loop pg newMem x

and findNode pg mem curr =
        match pg with 
        | (n1, BooleanAction(b), n2)::rest when n1 = curr && evalBool b mem -> (n2, mem)
        | (n1, BooleanAction(b), n2)::rest when n1 = curr && not(evalBool b mem) -> findNode rest mem curr
        | (n1, SkipAction, n2)::rest when n1 = curr -> (n2, mem)
        | (n1, action, n2)::rest when n1 = curr -> (n2, (doAction action mem))
        | (n1, action, n2)::rest when n1 <> curr -> findNode rest mem curr
        | [] -> failwith "stuck"   
        | _ -> failwith "cannot match"                                  
and doAction action (mem : Mem) = 
    match action with
    | AssignAction(name, value) ->  let name = (Variable(name))
                                    match mem.TryFind name with
                                    | Some v -> mem.Add(name, getValue value mem)
                                    | None -> mem.Add(name, getValue value mem)
    | ArrayAssignAction(name, index, value) -> let name = (Array(name,getValue index mem))
                                               match mem.TryFind name with
                                               | Some v -> mem.Add(name, getValue value mem)
                                               | None -> mem.Add(name,getValue value mem)
    | SkipAction -> mem
    | _ -> failwith "undefined"
and getValue expr (mem : Mem) =
    match expr with
    | Var(x)                                        -> let i = Map.tryFind (Variable(x)) mem
                                                       match i with
                                                       | Some(v) -> v
                                                       | None -> failwith "variable dosnt exist"
    | Num(x)                                        -> x
    | UPlusExpr(l)                                  -> getValue l mem
    | UMinusExpr(l)                                 -> -(getValue l mem)
    | TimesExpr(l, r)                               ->  getValue l mem * getValue r mem
    | DivExpr(l,r)                                  ->  getValue l mem / getValue r mem
    | PlusExpr(l,r)                                 ->  getValue l mem + getValue r mem
    | MinusExpr(l,r)                                -> getValue l mem - getValue r mem
    | PowExpr(l,r)                                  -> Power (getValue l mem) (getValue r mem)
    | ModExpr(l,r)                                  -> getValue l mem % getValue r mem
and evalBool expr mem = 
    match expr with
    | Bool(x) -> x
    | EqualExpr(l,r)                                ->  getValue l mem = getValue r mem
    | NotEqualExpr(l,r)                             ->  getValue l mem <> getValue r mem
    | GreaterExpr(l,r)                              ->  getValue l mem > getValue r mem
    | SmallerExpr(l,r)                              ->  getValue l mem < getValue r mem
    | EqGreaterExpr(l,r)                            ->  getValue l mem >= getValue r mem
    | EqSmallerExpr(l,r)                            ->  getValue l mem <= getValue r mem
    | OrExpr(l,r)                                   ->  evalBool l mem || evalBool r mem
    | AndExpr(l,r)                                  ->  (evalBool l mem && evalBool r mem) || (evalBool r mem && evalBool l mem)  
    | CondOrExpr(l,r)                               ->  evalBool l mem || evalBool r mem
    | CondAndExpr(l,r)                              ->  evalBool l mem && evalBool r mem  
    | NotExpr(l)                                    ->  not(evalBool l mem)
and Power x y =
    match y with
    | 0 -> 1
    | y when y<0 -> failwith "negativ pow"
    | y when y>0 -> Power (x*y) (y-1)
let rec initMem gcl mem =
    match gcl with
    | AssignExpr(s,expr) -> Map.add (Variable s) (getValue expr mem) mem
    | ArrayAssignExpr(s, index, expr) -> Map.add (Array (s, getValue index mem)) (getValue expr mem) mem
    | SemiColonExpr(c1,c2) -> let mem1 = initMem c1 mem
                              initMem c2 mem1
    | _ -> failwith "undefined"

// QUESTIONS
// basic hvordan det skal fungere? hvordan skal  den opbygges/retunere/læse init values?
// Hvordan skal if/do expressions læses forskelligt?
// Defnition af arrays? 
// Hvordan skal den nå et "slutpunkt", samt vide hvilken node den er nået til når det er GCL og den ikke er compilet
// skal compileren bruges?
// Hvordan skal "steps" laves?
// | & hvordan laves de i fsharp

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
    | (Node(n1),action,Node(n2))::t           -> string (n1 + "->" + n2 + "[label= \"" + PrintAction action + "\"]") + graphString t
    | []                                          -> ""
and PrintAction action =
    match action with
    | AssignAction(a,e) -> a + ":=" + prettyPrinterA e
    | ArrayAssignAction(a,e1,e2) -> a + "[" + (prettyPrinterA e1) + "]:=" + (prettyPrinterA e2)
    | BooleanAction(b) -> prettyPrinterB b
    | SkipAction -> "skip"
// We implement here the function that interacts with the user
let rec computeAST n =
   if n = 0 then
        printfn "Bye bye"
    else
        

        try
            // We parse the input string
            printf "Enter flag "
            let flag = Console.ReadLine();
            if (flag = "Det") then printf "You chose Det " else printf "You chose Non-Det \n"
            printf "Enter a GCL expression: "
            let e = parse (Console.ReadLine())
            let startmem = parse (Console.ReadLine())
            // and print the result of evaluating it
         
            //TEST do x>0 -> y:=x*y [] x>0 -> y:=x*y od

            if (flag = "Det") then printfn "The new set: %O" (compilerCDET e (Node("start")) (Node("slut")) 0) else printfn "The new set: %O" (compilerCNonDet e (Node("start")) (Node("slut")) 0)
            
            //Create graph file
            if (flag = "Det") then File.WriteAllText("PG.dot", "digraph {" + graphString (compilerCDET e (Node("start")) (Node("slut")) 0) + "}")
            else File.WriteAllText("PG.dot", "digraph {" + graphString (compilerCNonDet e (Node("start")) (Node("slut")) 0) + "}")

            printfn "initmem: %A" ((initMem startmem Map.empty))
            let memout = loop (compilerCDET e (Node("start")) (Node("slut")) 0) (initMem startmem Map.empty) (Node("start"))
            printfn "Result: %s" (prettyPrinterC (e)) //PrettyPrinter
            printfn "mem: %A" (memout)
            
            
            computeAST 1
        with err -> printfn "Invalid input"
                    computeAST 1

// Start interacting with the user
computeAST 1