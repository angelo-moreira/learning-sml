
(* Not tail optimised because the else waits for the result before computing the result
creating more stack frames before it multiplies the return value (n*fact) *)
fun fact n = if n=0 then 1 else n*fact (n-1)
val x = fact (3)


fun fact_tail n = 
    let fun aux (n, acc) =
        if n=0
        then acc
        else aux (n-1, acc*n)
    in aux (n, 1) end

val x2 = fact_tail (3)



fun sum xs =
    case xs of
        [] => 0
        | x::xs' => x + sum xs'

fun sum_tail xs =
    let fun aux (xs, acc) =
        case xs of
            [] => acc
            | x::xs' => aux (xs', x+acc)
    in
        aux (xs, 0)
    end


fun reverse xs =
    case xs of
        [] => []
        | x::xs' => (reverse xs') @ [x]

fun reverse_tail xs =
    let fun aux (xs, acc) =
        case xs of
            [] => acc
            | x::xs' => aux (xs', x::acc)
    in aux (xs, []) end
