
datatype pattern = Wildcard
     | Variable of string
     | UnitP
     | ConstP of int
     | TupleP of pattern list
     | ConstructorP of string * pattern


fun g f1 f2 p =
    let 
        val r = g f1 f2 
    in
        case p of
            Wildcard            => f1 ()
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

(* Set up useful stuff for later *)
exception NoAnswer

infix !>
fun x !> f = f x

fun only_capitals sl = List.filter (fn x => String.sub(x, 0) !> Char.isUpper) sl
fun longest_string1 xs = List.foldl (fn (x, y) => if size x > size y then x else y) "" xs;
fun longest_string2 xs = List.foldl (fn (x, y) => if size x >= size y then x else y) "" xs;

fun longest_string_helper f xs s = List.foldl (fn (x, y) => if f (size x,size y) = true then x else y) s xs

val longest_string3 = fn xs => longest_string_helper (fn (x,y) => x > y) xs ""
val longest_string4 = fn xs => longest_string_helper (fn (x,y) => x >= y) xs ""

val longest_capitalized = longest_string3 o only_capitals

val rev_string = implode o rev o explode

fun first_answer f xs = 
    case xs of
        [] => raise NoAnswer
        | x::xs' => case f x of SOME i => i | NONE => first_answer f xs'

fun all_answers f xs =
    let
        fun get_answer f xs acc =
            case xs of
                [] => SOME acc
                | x::xs' => case f x of
                                SOME i => get_answer f xs' (acc@i)
                                | NONE => NONE
    in
        get_answer f xs []
    end


fun count_wildcards (p: pattern) = g (fn x => 1) (fn x => 0) p
fun count_wild_and_variable_lengths (p: pattern) = g (fn x => 1) (fn x => size (x)) p

fun count_some_var (s, p) = g (fn x => 0) (fn x => if x = s then 1 else 0) p

fun all_variable_names p = 
    case p of
        Variable x  => [x]
        | TupleP ps => List.foldl (fn (p, acc) => all_variable_names(p)@acc) [] ps
        | _         => []

(* first version is shorter but less efficient since filter doesnt stop when true, I wonder how I could pipe this 1st version *)
(* fun has_duplicates xs = List.exists (fn x => List.length (List.filter (fn y => y=x) xs) > 1) xs *)
fun has_duplicates (x::xs) = if (List.exists (fn y => x=y) xs) = true then false else has_duplicates (xs)
| has_duplicates ([]) = true

fun check_pat (p:pattern) = all_variable_names p !> has_duplicates

(* datatype pattern = Wildcard
     | Variable of string
     | UnitP
     | ConstP of int
     | TupleP of pattern list
     | ConstructorP of string * pattern *)

(* fun g f1 f2 p =
    let 
        val r = g f1 f2 
    in
        case p of
            Wildcard            => f1 ()
            | Variable x        => f2 x
            | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
            | ConstructorP(_,p) => r p
            | _                 => 0
    end *)


datatype valu = Const of int
        | Unit
        | Tuple of valu list
        | Constructor of string * valu

(* fun get_all_answers f xs = case all_answers f xs of SOME i => i |_ => [] *)
(* val filter_pattern = fn x => 
    case x of 
        (Variable p, v) => [(v, p)]
        | (TupleP p, Tuple v)   => match (v,p)
        |_              => [] *)

(* fun match ((va: valu, pa: pattern)) = 
    case (va, pa) of
        (v, Variable p)       => SOME [(p, v)]
        (Tuple v, TupleP p) => get_all_answers (fn x => ) (ListPair.zip (p, v)) *)

