module Exam2022
(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but 
   it does allow you to work in interactive mode and you can just remove the '=' 
   to make the project compile again.

   You will also need to load JParsec.fs. Do this by typing
   #load "JParsec.fs" 
   in the interactive environment. You may need the entire path.

   Do not remove the module declaration (even though that does work) because you may inadvertently
   introduce indentation errors in your code that may be hard to find if you want
   to switch back to project mode. 

   Alternative, keep the module declaration as is, but load ExamInteractive.fsx into the interactive environment
   *)
(*
 module Exam2022 = 
 *)

(* 1: Grayscale images *)
    type grayscale =
    | Square of uint8
    | Quad of grayscale * grayscale * grayscale * grayscale
    
    let img = 
      Quad (Square 255uy, 
            Square 128uy, 
            Quad(Square 255uy, 
                 Square 128uy, 
                 Square 192uy,
                 Square 64uy),
            Square 0uy)
    
(* Question 1.1 *)
    let rec countWhite (img: grayscale) =
        match img with
        | Square(x) -> if x = 255uy then 1 else 0
        | Quad(s1, s2, s3, s4) -> countWhite s1 + countWhite s2 + countWhite s3 + countWhite s4
    
(* Question 1.2 *)
    let rec rotateRight (img: grayscale) =
        match img with
        | Square x -> Square x
        | Quad(s1, s2, s3, s4) -> Quad(rotateRight s4,rotateRight s1,rotateRight s2,rotateRight s3)
        

(* Question 1.3 *)
    let rec map mapper (img: grayscale) =
        match img with
        | Square x -> mapper x
        | Quad(s1, s2, s3, s4) -> Quad(map mapper s1, map mapper s2, map mapper s3, map mapper s4)
    
    let bitmap (img: grayscale) = map(fun x -> if x <=127uy then Square 0uy else Square 255uy) img

(* Question 1.4 *)

    let rec fold folder acc (img:grayscale) = 
        match img with
        | Square x -> folder acc x
        | Quad(s1, s2, s3, s4) -> fold folder (fold folder (fold folder (fold folder acc s1) s2) s3) s4   
    
    let countWhite2 (img: grayscale) = fold(fun acc elem -> if elem = 255uy then acc + 1 else acc) 0 img
                         

(* 2: Code Comprehension *)
    let rec foo =
        function
        | 0 -> ""
        | x when x % 2 = 0 -> foo (x / 2) + "0"
        | x when x % 2 = 1 -> foo (x / 2) + "1"

    let rec bar =
        function
        | []      -> []
        | x :: xs -> (foo x) :: (bar xs)

(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo and bar?

    A: 
    foo: int -> string
    bar: int list -> string list


    Q: What does the functions foo bar do.
       Focus on what it does rather than how it does it.

    A: 
    Foo turns an integer number into a string, the binary number representation of the integer number input.
    Bar turns a list of integer numbers into a string list their corresponding binary number representations.
    
    Q: What would be appropriate names for functions  
 
    A:
     foo: toBinary
     bar: toBinaryLst
        
    Q: The function foo does not return reasonable results for all possible inputs.
       What requirements must we have on the input to foo in order to get reasonable results?
    
    A: The input must be positive. Negative numbers return an error message and 0 returns "" which is not the binary equivalent
    *)
        

(* Question 2.2 *)

 
    (* 
    The function foo compiles with a warning. 

    
    Q: What warning and why?

    A: 
    The warning is "Incomplete pattern matches on this expression. For example, the value '1' may indicate a case not covered by the pattern(s). However, a pattern rule with a 'when' clause might successfully match this value"
   
    It happens because it cannot know whether the when part of the pattern match is exhaustive or not.

    *)

    let rec foo2 =
        function
        | 0 -> ""
        | x when x % 2 = 0 -> foo (x / 2) + "0"
        | x when x % 2 = 1 -> foo (x / 2) + "1"
        | _ -> "error. negative number"

(* Question 2.3 *) 

    let bar2 = List.map foo

(* Question 2.4 *)

    (*

    Q: Neither foo nor bar is tail recursive. Pick one (not both) of them and explain why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation.
       You need to make clear what aspects of the evaluation tell you that the function is not tail recursive.
       Keep in mind that all steps in an evaluation chain must evaluate to the same value
       ((5 + 4) * 3 --> 9 * 3 --> 27, for instance).

    A: <Your answer goes here>
    
    Q: Even though neither `foo` nor `bar` is tail recursive only one of them runs the risk of overflowing the stack.
       Which one and why does  the other one not risk overflowing the stack?

    A: It is bar that risks overflowing the stack since building the list can overflow the stack, whereas foo has integer overflow.

    *)
(* Question 2.5 *)
    
    (*  let rec foo2 =
        function
        | 0 -> ""
        | x when x % 2 = 0 -> foo (x / 2) + "0"
        | x -> foo (x / 2) + "1" *)

    let fooTail (a: int) =
        let rec aux acc a' =
            match a' with
            | 0 -> acc
            | x when x % 2 = 0 -> aux ("0" + acc) (x/2)
            | x -> aux ("1" + acc) (x/2)
        aux "" a

(* Question 2.6 *)
    
    (*
     let rec bar =
        function
        | []      -> []
        | x :: xs -> (foo x) :: (bar xs)
    *)
    let barTail (a: int list) =
        let rec aux cont a' =
            match a' with
            | [] -> cont []
            | x :: xs -> aux (fun result -> cont (fooTail x :: result)) xs
        aux id a
           
        
    
        
        

(* 3: Matrix operations *)

    type matrix = int[,]

    let init f rows cols = Array2D.init rows cols f

    let numRows (m : matrix) = Array2D.length1 m
    let numCols (m : matrix) = Array2D.length2 m

    let get (m : matrix) row col = m.[row, col]
    let set (m : matrix) row col v = m.[row, col] <- v

    let print (m : matrix) =
        for row in 0..numRows m - 1 do
            for col in 0..numCols m - 1 do
                printf "%d\t" (get m row col)
            printfn ""

(* Question 3.1 *)

    let failDimensions (m1: matrix) (m2: matrix) =
        (sprintf "Invalid matrix dimensions: m1 rows = %A, m1 columns = %A, m2 rows = %A, m2 columns = %A" (numRows m1) (numCols m1) (numRows m2) (numCols m2))
        |> failwith

(* Question 3.2 *)

    let add (m1: matrix) (m2: matrix) =
        let (a,b) = (numRows m1,numCols m1)
        let (c,d) = (numRows m2,numCols m2)
        if (a,b) <> (c,d)
        then failDimensions m1 m2
        else init (fun i j -> (get m1 i j) + (get m2 i j)) a b
        

(* Question 3.3 *)
    
    let m1 = (init (fun i j -> i * 3 + j + 1) 2 3) 
    let m2 = (init (fun j k -> j * 2 + k + 1) 3 2)

    let dotProduct (m1: matrix) (m2: matrix) (row: int) (col: int) =
        let rec aux (x: int) =
            match x with
            | -1 -> 0
            | x -> (get m1 row x * get m2 x col) + aux (x-1)
        aux (numCols m2)
            
        
    let mult (m1: matrix) (m2: matrix) =
        let (a,b) = (numRows m1,numCols m1)
        let (c,d) = (numRows m2,numCols m2)
        if a <> d
        then failDimensions m1 m2
        else init (fun i j -> dotProduct m1 m2 i j) (numRows m1) (numCols m2)
        

(* Question 3.4 *)
    let parInit _ = failwith "not implemented"

(* 4: Stack machines *)

    type cmd = Push of int | Add | Mult
    type stackProgram = cmd list

(* Question 4.1 *)

    type stack = S of int list
    
    let emptyStack () = S[]

(* Question 4.2 *)

    let  runStackProgram (prog: stackProgram) =
        let rec aux stack prog' =
            match prog', stack with
            |Add :: xs, S(x::y::ys) -> aux (S(x+y :: ys)) xs
            |Mult :: xs, S(x::y::ys) -> aux (S(x*y :: ys)) xs
            |Push x :: xs, S(s)  -> aux (S(x::s)) xs
            |[], S(x :: xs) -> x
            | _ -> failwith "empty stack"
        aux (emptyStack ())
      
            
            
        

(* Question 4.3 *)
    
    type StateMonad<'a> = SM of (stack -> ('a * stack) option)

    let ret x = SM (fun s -> Some (x, s))
    let fail  = SM (fun _ -> None)
    let bind f (SM a) : StateMonad<'b> = 
        SM (fun s -> 
            match a s with 
            | Some (x, s') -> 
                let (SM g) = f x             
                g s'
            | None -> None)
        
    let (>>=) x f = bind f x
    let (>>>=) x y = x >>= (fun _ -> y)

    let evalSM (SM f) = f (emptyStack ())

    let push (x: int) =
        SM(fun (S list) -> Some((), x :: list |> S))
    let pop = SM(
        fun (S list) ->
            match list with
            | [] -> None
            | x :: xs -> Some(x, xs |> S)
            )

(* Question 4.4 *)

    type StateBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = new StateBuilder()

    (*
        let  runStackProgram (prog: stackProgram) =
        let rec aux stack prog' =
            match prog', stack with
            |Add :: xs, S(x::y::ys) -> aux (S(x+y :: ys)) xs
            |Mult :: xs, S(x::y::ys) -> aux (S(x*y :: ys)) xs
            |Push x :: xs, S(s)  -> aux (S(x::s)) xs
            |[], S(x :: xs) -> x
            | _ -> failwith "empty stack"
        aux (emptyStack ())
      
    *)
    let runStackProg2 (prog: stackProgram) =
        let rec aux prog' =
            match prog' with
            | Add :: xs -> pop >>= (fun x -> pop >>= (fun y -> push (x+y) >>>= aux xs))
            | Mult :: xs -> pop >>= (fun x -> pop >>= (fun y -> push (x*y) >>>= aux xs))
            | Push x :: xs -> push x >>>= (aux xs)
            | [] -> pop >>= (fun y -> push y >>>= ret y)
        aux prog
        
(* Question 4.5 *)

    let parseStackProg _ = failwith "not implemented"