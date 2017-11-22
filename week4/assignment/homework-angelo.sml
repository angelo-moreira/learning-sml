
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
                [] => acc
                | x::xs' => case f x of
                                SOME i => get_answer f xs' (acc@i)
                                | NONE => get_answer f xs' acc
    in
        case (get_answer f xs []) of
            [] => NONE
            | all_xs => SOME all_xs
    end

