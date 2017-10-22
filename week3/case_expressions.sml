use "datatypes.sml";
(* The return type must be the same otherwise we get a type error *)

fun get_food (food) = 
    case food of
        Name n => 1
        | Date d1 => 2
        | Price p => 3
        | Pizza => 4
        | Ingredients (ingredients) => 5;

datatype exp = Constant of int
            | Negate of exp
            | Add of exp * exp
            | Multiply of exp * exp

(* Recursive functions is the standard when you want recursive data *)
fun eval e =
    case e of
          Constant i        => i
        | Negate e2         => ~ (eval e2)
        | Add (e1, e2)      => (eval e1) + (eval e2)
        | Multiply (e1, e2) => (eval e1) * (eval e2)

val test = Add (Constant (10+5), Negate (Constant (5)))

fun max_constant e =
    case e of
        Constant i => i
        | Negate (e2) => max_constant (e2)
        | Add (e1, e2) => Int.max (max_constant e1, max_constant e2)
        | Multiply (e1, e2) => Int.max (max_constant e1, max_constant e2)

