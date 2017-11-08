
(* Higher order functions dont need to be polymorphic *)
fun times_until_zero (f,x) =
    if x = 0 then 0 else 1 + times_until_zero (f, f x)

(* An example of a higher order function that is polymorphic *)
fun len xs =
    case xs of
        [] => 0
        |_::xs' => 1 + len xs'

