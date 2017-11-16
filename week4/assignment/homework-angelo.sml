(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
     | Variable of string
     | UnitP
     | ConstP of int
     | TupleP of pattern list
     | ConstructorP of string * pattern

datatype valu = Const of int
        | Unit
        | Tuple of valu list
        | Constructor of string * valu

fun g f1 f2 p =
    let 
        val r = g f1 f2 
    in
        case p of
            Wildcard          => f1 ()
            | Variable x        => f2 x
            | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
            | ConstructorP(_,p) => r p
            | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
       | UnitT
       | IntT
       | TupleT of typ list
       | Datatype of string

(**** you can put all your code here ****)
infix !>
fun x !> f = f x

fun only_capitals sl = List.filter (fn x => String.sub(x, 0) !> Char.isUpper) sl
fun longest_string1 xs = List.foldl (fn (x, y) => if String.size (x) > String.size (y) then x else y) "" xs;
fun longest_string2 xs = List.foldl (fn (x, y) => if String.size (x) >= String.size (y) then x else y) "" xs;

fun longest_string_helper f xs s = List.foldl (fn (x, y) => if f (String.size(x),String.size(y)) = true then x else y) s xs

val longest_string3 = fn xs => longest_string_helper (fn (x,y) => x > y) xs ""
val longest_string4 = fn xs => longest_string_helper (fn (x,y) => x >= y) xs ""

val longest_capitalized = longest_string3 o only_capitals

exception NoAnswer
fun first_answer f xs = 
        (List.find (fn x => case f(x) of SOME _ => true |_ => false ) xs) 
        !> (fn x => case x of SOME i => i |_ => raise NoAnswer)


