
fun full_name (r: {first: string, middle: string, last: string}) =
    case r of
        {first=x, middle=y, last=z} => x ^ " " ^ y ^ " " ^ z

fun full_name2 (r: {first: string, middle: string, last: string}) =
    let val {first=x, middle=y, last=z} = r
    in
        x ^ " " ^ y ^ " " ^ z
    end

fun full_name3 { first=x, middle=y, last=z } =
    x ^ " " ^ y ^ " " ^ z


fun sum_triple1 (triple: int * int * int) =
    case triple of
        (x,y,z) => x + y + z

fun sum_triple2 triple =
    let val (x,y,z) = triple
    in
        x + y + z
    end

fun sum_triple3 (x,y,z) =
    x + y + z

(* in ML every function only receives 1 parameter, it then pattern matches against the rest of the parameters *)