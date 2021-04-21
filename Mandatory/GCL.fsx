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
    | GCLoopExpr(e1,e2)                             -> OrExpr((computeB e2), (computeB e1))


// Interpter -------------
//loops the pg
let rec loop pg mem curr =  let (x, newMem) = findNode pg mem curr
                            match x with
                            | (Node("slut")) -> mem 
                            | _ -> loop pg newMem x

and findNode pg mem curr = //finds the node we are currently on in the pg and does the action
        match pg with 
        | (n1, BooleanAction(b), n2)::rest when n1 = curr && evalBool b mem -> (n2, mem)
        | (n1, BooleanAction(b), n2)::rest when n1 = curr && not(evalBool b mem) -> findNode rest mem curr
        | (n1, SkipAction, n2)::rest when n1 = curr -> (n2, mem)
        | (n1, action, n2)::rest when n1 = curr -> (n2, (doAction action mem))
        | (n1, action, n2)::rest when n1 <> curr -> findNode rest mem curr
        | [] -> failwith "stuck"   
        | _ -> failwith "cannot match"                                  
and doAction action (mem : Mem) = //does assign actions
    match action with
    | AssignAction(name, value) ->  let name = (Variable(name))
                                    match mem.TryFind name with
                                    | Some v -> mem.Add(name, getValue value mem)
                                    | None -> failwith "stuck"  
    | ArrayAssignAction(name, index, value) -> let name = (Array(name,getValue index mem))
                                               match mem.TryFind name with
                                               | Some v -> mem.Add(name, getValue value mem)
                                               | None -> failwith "stuck"  
    | SkipAction -> mem //skip
    | _ -> failwith "undefined"
and getValue expr (mem : Mem) = //gets a value from arithmetic expression
    match expr with
    | Var(x)                                        -> let i = Map.tryFind (Variable(x)) mem
                                                       match i with
                                                       | Some(v) -> v
                                                       | None -> failwith "variable dosnt exist"
    | ArrayExpr(s,a)                                -> let i = Map.tryFind (Array(s,getValue a mem)) mem
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
    | OrExpr(l,r)                                   ->  (evalBool l mem || evalBool r mem) && (evalBool r mem || evalBool l mem)  //ensure non-shortcut
    | AndExpr(l,r)                                  ->  (evalBool l mem && evalBool r mem) || (evalBool r mem && evalBool l mem)  //ensure non-shortcut
    | CondOrExpr(l,r)                               ->  evalBool l mem || evalBool r mem
    | CondAndExpr(l,r)                              ->  evalBool l mem && evalBool r mem  
    | NotExpr(l)                                    ->  not(evalBool l mem)
and Power x y = //power 
    match y with
    | 0 -> 1
    | y when y<0 -> failwith "negativ pow"
    | _ -> Power (x*y) (y-1)
let rec initMem gcl mem = //generate memory when typed on form x:=1;y:=0 ...
    match gcl with
    | AssignExpr(s,expr) -> Map.add (Variable s) (getValue expr mem) mem
    | ArrayAssignExpr(s, index, expr) -> Map.add (Array (s, getValue index mem)) (getValue expr mem) mem
    | SemiColonExpr(c1,c2) -> let mem1 = initMem c1 mem
                              initMem c2 mem1
    | _ -> failwith "undefined"



let PlusTable v1 v2 = //plus table
    match (v1,v2) with
    | (Plus,Plus) -> Set.ofList [Plus]
    | (Plus, Zero) -> Set.ofList[Plus]
    | (Plus, Minus) -> Set.ofList[Plus;Minus;Plus]
    | (Zero, Plus) -> Set.ofList[Plus]
    | (Zero, Zero) -> Set.ofList[Zero]
    | (Zero, Minus) -> Set.ofList[Minus]
    | (Minus, Minus) -> Set.ofList[Minus]
    | (Minus, Zero) -> Set.ofList[Minus]
    | (Minus, Plus) -> Set.ofList [Plus;Minus;Plus]
let MinusTable v1 v2 = //minus table
    match (v1,v2) with
    | (Plus,Plus) -> Set.ofList[Plus;Minus;Plus]
    | (Plus, Zero) -> Set.ofList [Plus]
    | (Plus, Minus) -> Set.ofList[Plus]
    | (Zero, Plus) -> Set.ofList[Minus]
    | (Zero, Zero) -> Set.ofList[Zero]
    | (Zero, Minus) -> Set.ofList[Plus]
    | (Minus, Minus) -> Set.ofList[Plus;Minus;Plus]
    | (Minus, Zero) -> Set.ofList[Minus]
    | (Minus, Plus) -> Set.ofList[Minus]
let DivideTable v1 v2 = //minus table
    match (v1,v2) with
    | (Plus,Plus) -> Set.ofList[Plus]
    | (Plus, Zero) -> failwith "divide 0"
    | (Plus, Minus) -> Set.ofList[Minus]
    | (Zero, Plus) -> Set.ofList[Zero]
    | (Zero, Zero) -> failwith "divide 0"
    | (Zero, Minus) -> Set.ofList[Zero]
    | (Minus, Minus) -> Set.ofList[Plus]
    | (Minus, Zero) -> failwith "divide 0"
    | (Minus, Plus) -> Set.ofList[Minus]
let MultiplyTable v1 v2 = //minus table
    match (v1,v2) with
    | (Plus,Plus) -> Set.ofList [Plus]
    | (Plus, Zero) -> Set.ofList[Zero]
    | (Plus, Minus) -> Set.ofList[Minus]
    | (Zero, Plus) -> Set.ofList[Zero]
    | (Zero, Zero) -> Set.ofList[Zero]
    | (Zero, Minus) -> Set.ofList[Zero]
    | (Minus, Minus) -> Set.ofList[Plus]
    | (Minus, Zero) -> Set.ofList[Zero]
    | (Minus, Plus) -> Set.ofList[Minus]
let PowTable v1 v2 = //minus table
    match (v1,v2) with
    | (Plus,Plus) -> Set.ofList[Plus]
    | (Plus, Zero) -> Set.ofList[Plus]
    | (Plus, Minus) -> Set.ofList[Plus]
    | (Zero, Plus) -> Set.ofList[Zero]
    | (Zero, Zero) -> Set.ofList[Plus]
    | (Zero, Minus) -> Set.ofList[Zero]
    | (Minus, Minus) -> Set.ofList[Plus;Minus]
    | (Minus, Zero) -> Set.ofList[Plus]
    | (Minus, Plus) -> Set.ofList[Plus;Minus]
let ModTable v1 v2 = //minus table
    match (v1,v2) with
    | (Plus,Plus) -> Set.ofList[Plus]
    | (Plus, Zero) -> Set.ofList[Plus]
    | (Plus, Minus) -> Set.ofList[Plus]
    | (Zero, Plus) -> Set.ofList[Zero]
    | (Zero, Zero) -> Set.ofList[Plus]
    | (Zero, Minus) -> Set.ofList[Zero]
    | (Minus, Minus) -> Set.ofList[Plus;Minus]
    | (Minus, Zero) -> Set.ofList[Plus]
    | (Minus, Plus) -> Set.ofList[Plus;Minus]


let rec computeSignArith (aMem1, aMem2) expr =
    match expr with
    | Num(x) -> Set.ofList [sign x]
    | Var(x) -> let i = Map.tryFind x aMem1
                match i with
                | Some v -> Set.ofList [v]
                | None -> failwith "TODO"
    | ArrayExpr(s,a) -> let check = (computeSignArith (aMem1, aMem2) a)
                        if check.Contains(Zero) || check.Contains(Plus) then
                        let i = Map.tryFind s aMem2
                        match i with
                        | Some v -> v
                        | None -> failwith "TODO"
                        else failwith "TODO"
                   

                           //contains {0 +} then do :
    | TimesExpr(l, r)                               -> let set1 = (computeSignArith (aMem1, aMem2) l)
                                                       let set2 = (computeSignArith (aMem1, aMem2) l)
                                                       Set.fold (fun acc elem -> Set.union acc (Set.fold (fun acc2 elem2 -> Set.union acc2 (MultiplyTable elem elem2)) Set.empty set2)) Set.empty set1
    | DivExpr(l,r)                                  -> let set1 = (computeSignArith (aMem1, aMem2) l)
                                                       let set2 = (computeSignArith (aMem1, aMem2) l)
                                                       Set.fold (fun acc elem -> Set.union acc (Set.fold (fun acc2 elem2 -> Set.union acc2 (DivideTable elem elem2)) Set.empty set2)) Set.empty set1
    | PlusExpr(l,r)                                 -> let set1 = (computeSignArith (aMem1, aMem2) l)
                                                       let set2 = (computeSignArith (aMem1, aMem2) l)
                                                       Set.fold (fun acc elem -> Set.union acc (Set.fold (fun acc2 elem2 -> Set.union acc2 (PlusTable elem elem2)) Set.empty set2)) Set.empty set1
    | MinusExpr(l,r)                                -> let set1 = (computeSignArith (aMem1, aMem2) l)
                                                       let set2 = (computeSignArith (aMem1, aMem2) l)
                                                       Set.fold (fun acc elem -> Set.union acc (Set.fold (fun acc2 elem2 -> Set.union acc2 (MinusTable elem elem2)) Set.empty set2)) Set.empty set1
    | PowExpr(l,r)                                  -> let set1 = (computeSignArith (aMem1, aMem2) l)
                                                       let set2 = (computeSignArith (aMem1, aMem2) l)
                                                       Set.fold (fun acc elem -> Set.union acc (Set.fold (fun acc2 elem2 -> Set.union acc2 (PowTable elem elem2)) Set.empty set2)) Set.empty set1
    | ModExpr(l,r)                                  -> let set1 = (computeSignArith (aMem1, aMem2) l)
                                                       let set2 = (computeSignArith (aMem1, aMem2) l)
                                                       Set.fold (fun acc elem -> Set.union acc (Set.fold (fun acc2 elem2 -> Set.union acc2 (ModTable elem elem2)) Set.empty set2)) Set.empty set1
and sign x = 
    match x with
    | x when x>0 -> Plus
    | x when x<0 -> Minus
    | x when x=0 -> Zero


let EqualTable v1 v2 =
    match (v1, v2) with
    | (Plus,Plus) -> Set.ofList[true]
    | (Plus, Zero) -> Set.ofList[false]
    | (Plus, Minus) -> Set.ofList[false]
    | (Zero, Plus) -> Set.ofList[false]
    | (Zero, Zero) -> Set.ofList[true]
    | (Zero, Minus) -> Set.ofList[false]
    | (Minus, Minus) -> Set.ofList[true]
    | (Minus, Zero) -> Set.ofList[false]
    | (Minus, Plus) -> Set.ofList[false]
let GreaterTable v1 v2 =
    match (v1, v2) with
    | (Plus,Plus) -> Set.ofList[true; false]
    | (Plus, Zero) -> Set.ofList[true]
    | (Plus, Minus) -> Set.ofList[true]
    | (Zero, Plus) -> Set.ofList[false]
    | (Zero, Zero) -> Set.ofList[false]
    | (Zero, Minus) -> Set.ofList[true]
    | (Minus, Minus) -> Set.ofList[true; false]
    | (Minus, Zero) -> Set.ofList[false]
    | (Minus, Plus) -> Set.ofList[false]
let GreaterEqualTable v1 v2 =
    match (v1, v2) with
    | (Plus,Plus) -> Set.ofList[true; false]
    | (Plus, Zero) -> Set.ofList[true]
    | (Plus, Minus) -> Set.ofList[true]
    | (Zero, Plus) -> Set.ofList[false]
    | (Zero, Zero) -> Set.ofList[false; true]
    | (Zero, Minus) -> Set.ofList[true]
    | (Minus, Minus) -> Set.ofList[true; false]
    | (Minus, Zero) -> Set.ofList[false]
    | (Minus, Plus) -> Set.ofList[false]
let OrTable v1 v2 =
    match (v1, v2) with
    | (true,true) -> Set.ofList[true]
    | (false, true) -> Set.ofList[true]
    | (true, false) -> Set.ofList[true]
    | (false, false) -> Set.ofList[false]
let AndTable v1 v2 =
    match (v1, v2) with
    | (true,true) -> Set.ofList[true]
    | (false, true) -> Set.ofList[false]
    | (true, false) -> Set.ofList[false]
    | (false, false) -> Set.ofList[false]
let NotTable b =
    match b with
    | false -> Set.ofList[true]
    | true -> Set.ofList[false]

let rec computeSignBool (aMem1, aMem2) expr =
    match expr with
    | Bool(true) -> Set.ofList[true]
    | Bool(false) -> Set.ofList[false]
    | EqualExpr(a1,a2) ->   let set1 = computeSignArith (aMem1, aMem2) a1
                            let set2 = computeSignArith (aMem1, aMem2) a2
                            Set.fold (fun acc elem -> Set.union acc (Set.fold (fun acc2 elem2 -> Set.union acc2 (EqualTable elem elem2)) Set.empty set2)) Set.empty set1
    | GreaterExpr(a1,a2)->  let set1 = computeSignArith (aMem1, aMem2) a1
                            let set2 = computeSignArith (aMem1, aMem2) a2
                            Set.fold (fun acc elem -> Set.union acc (Set.fold (fun acc2 elem2 -> Set.union acc2 (GreaterTable elem elem2)) Set.empty set2)) Set.empty set1
    | EqGreaterExpr(a1,a2)->  let set1 = computeSignArith (aMem1, aMem2) a1
                              let set2 = computeSignArith (aMem1, aMem2) a2
                              Set.fold (fun acc elem -> Set.union acc (Set.fold (fun acc2 elem2 -> Set.union acc2 (GreaterEqualTable elem elem2)) Set.empty set2)) Set.empty set1
    | OrExpr(b1, b2) -> let set1 = computeSignBool (aMem1, aMem2) b1
                        let set2 = computeSignBool (aMem1, aMem2) b2
                        Set.fold (fun acc elem -> Set.union acc (Set.fold (fun acc2 elem2 -> Set.union acc2 (OrTable elem elem2)) Set.empty set2)) Set.empty set1 
    | AndExpr(b1,b2) -> let set1 = computeSignBool (aMem1, aMem2) b1
                        let set2 = computeSignBool (aMem1, aMem2) b2
                        Set.fold (fun acc elem -> Set.union acc (Set.fold (fun acc2 elem2 -> Set.union acc2 (AndTable elem elem2)) Set.empty set2)) Set.empty set1
    | NotExpr(b) -> let set1 = computeSignBool (aMem1, aMem2) b
                    Set.fold (fun acc elem -> Set.union acc (NotTable elem)) Set.empty set1 


let rec detectSignAction (aMem1, aMem2) expr M = 
        match expr with
        | AssignExpr(s,a) -> 
        | ArrayAssignExpr(s,a1,a2) ->

//Create table functions
//make make big U on page 52
 
//question array
//qeustion uplus and uminus
//question on output     

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
            printfn "Result: %s" (prettyPrinterC (e)) //PrettyPrinter
            let memout = loop (compilerCDET e (Node("start")) (Node("slut")) 0) (initMem startmem Map.empty) (Node("start"))
          
            printfn "mem: %A" (memout)
            
            
            computeAST 1
        with Failure(msg) -> printfn "%s" msg
                             computeAST 1

// Start interacting with the user
computeAST 1