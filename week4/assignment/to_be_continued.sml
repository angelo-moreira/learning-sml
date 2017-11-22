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

(* first version is shorter but less efficient since filter doesnt stop when true, I wonder how I could pipe this 1st version *)
(* fun has_duplicates xs = List.exists (fn x => List.length (List.filter (fn y => y=x) xs) > 1) xs *)
fun has_duplicates (x::xs) = if (List.exists (fn y => x=y) xs) = true then true else has_duplicates (xs)
| has_duplicates ([]) = false

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

fun match ((va: valu, pa: pattern)) = 
    case (va, pa) of
        (* (Tuple v, TupleP p) => ListPair.zip (p, v) *)
        (* (v, Variable p)       => SOME [(p, v)] *)
        (Tuple v, TupleP p) => all_answers (fn x => case x of (Variable v, p) => SOME [(v, p)] |_ => NONE) (ListPair.zip (p, v))

        (* all_answers (fn x => case (#1 x) of Variable i => SOME [x] |_ => NONE) a; *)

        (* List.foldl (fn (v, p) => ) [] (v,p) *)
        (* | (Constructor x, y) => y *)

        (* | Constructor i p2 => ConstructorP i p2 *)

            (* case (ms, cs, sum_cards(cards_held) > g) of
                (_, c::cs, true) => score (cards_held, g) *)



(* val test11 = match (Const(1), UnitP) = NONE *)
