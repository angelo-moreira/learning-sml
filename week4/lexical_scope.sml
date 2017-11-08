
(* Lexical scope and how it differs from dynamic scope *)

val x = 1

fun f y = x + y

val x = 2

val y = 3

val z = f (x+y)
(* z maps to 6, because the function f was defined when x was 1 *)



val x =1
fun f y =
    let
        val x = y + 1
    in
        fn z => x + y + z
    end

val x = 3
val g = f 4 (* returns a function that adds 9 to its argument *)
val y = 5
val z = g 6 (* returns 15 *)

(* Another example with higher order functions lexical scope *)
fun f g =
    let
        val x = 3
    in
        g 2
    end

val x = 4

fun h y = x + y
val z = f h (* 6 *)