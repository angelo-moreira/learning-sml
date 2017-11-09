
(* Lexical scope binding variables to avoid recomputation *)
fun filter (f, xs) =
    case xs of
        [] => []
        | x::xs' =>  if f x
                    then x::(filter (f, xs'))
                    else filter (f, xs')


fun allShorterThan1 (xs, s) =
    filter (fn x => String.size x < (print "!"; String.size s), xs)

fun allShorterThan2 (xs, s) =
    let
        val i = (print "!"; String.size s)
    in
        filter (fn x => String.size x < i, xs)
    end


val _ = print "\nwithAllShorterThan1: "
val x1 = allShorterThan1 (["1", "333", "22", "4444"], "xxx")

val _ = print "\nwithAllShorterThan2: "
val x2 = allShorterThan2 (["1", "333", "22", "4444"], "xxx")

(* This demonstrated than we don't do recomputation of values when using bindings *)

(* Higher order functions are very reusable we can basically send all different data structures we fancy *)
fun fold (f, acc, xs) =
    case xs of
        [] => acc
        | x::xs' => fold (f, f(acc, x), xs')

fun sum_list xs = fold ((fn (x,y) => x+y), 0 , xs)
fun is_positive xs = fold ((fn (x,y) => x andalso y >= 0), true, xs)

(* Example using private data *)

(* Counting the number of elements between high and low inclusive *)
fun f3 (xs, low, high) =
    fold ((fn (x, y) => 
        x + (if y >= low andalso y <= high then 1 else 0)),
        0, xs)


(* see if the string length is bigger than a list of string *)
fun f4 (xs, s) =
    let
        val i = String.size s
    in
        fold ((fn (x, y) => x andalso String.size y < i), true, xs)
    end

(* Do all elements of the list produce true when passed to g *)
fun f5 (g, xs) = fold ((fn(x, y) => x andalso g y), true, xs)

fun f4again (xs, s) =
    let
        val i = String.size s
    in
        f5 (fn y => String.size y < i, xs)
    end

