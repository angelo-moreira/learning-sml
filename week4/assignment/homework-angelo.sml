
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
        (List.find (fn x => case f(x) of SOME _ => true |_ => false ) xs) 
        !> (fn x => case x of SOME i => i |_ => raise NoAnswer)

fun all_answers f xs =
        case List.filter (fn x => case f x of SOME _ => true |_ => false) xs of
        [] => NONE
        | cs => SOME cs


fun count_wildcards (p: pattern) = g (fn x => 1) (fn x => 0) p
fun count_wild_and_variable_lengths (p: pattern) = g (fn x => 1) (fn x => size (x)) p

fun count_some_var (s, p) = g (fn x => 0) (fn x => if x = s then 1 else 0) p

fun all_variable_names p = 
    case p of
        Variable x  => [x]
        | TupleP ps => List.foldl (fn (p, acc) => all_variable_names(p)@acc) [] ps
        | _         => []

fun has_duplicates xs = List.exists (fn x => List.length (List.filter (fn y => y=x) xs) > 1) xs
fun check_pat (p:pattern) = all_variable_names p !> has_duplicates

