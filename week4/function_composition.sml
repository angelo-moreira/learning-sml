
fun compose (f, g) = fn x => f (g, x)

fun sqrt_of_abs i = Math.sqrt (Real.fromInt (abs i))
fun sqrt_of_abs i = (Math.sqrt o Real.fromInt o abs) i
val sqrt_of_abs = Math.sqrt o Real.fromInt o abs

(* We can create our operators with infix, I dont really understand how it has been defined *)
infix !>

fun x !> f = f x

fun sqrt_of_abs i = i !> abs !> Real.fromInt !> Math.sqrt

(* Wow I think I "kinda" get it, you can chain stuff but we need to send functions because of the fun x definition above *)
4 !> (fn x => x+1) !> (fn y => y+2) !> (fn x => x+3);

