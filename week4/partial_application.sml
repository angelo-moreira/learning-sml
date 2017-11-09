
fun sorted3 x y z = z >= y andalso y >= x

fun fold f acc xs = (* means fun fold f = fn acc => fn xs => *)
    case xs of
        []     => acc
      | x::xs' => fold f (f(acc, x)) xs'

(* If a curried function has too few arguments it is then gonna return a function *)
val is_non_negative = sorted3 0 0
val sum = fold (fn (x,y) => x+y) 0

(* not using currying is a harder-to-notice version of unnecessary function wrapping *)
fun is_non_negative_inferior x = sorted3 0 0 x
fun sum_inferior xs = fold (fn (x, y) => x+y) 0 xs

(* another example of when to use currying *)

fun range i j = if i > j then [] else i::range (i+1) j
(* range 3 6 -> [3,4,5,6] *)
val countup = range 1

fun countup_inferior x = range 1 x



(* common style is to curry higher-order functions with function arguments, first to enable convenient partial application *)
fun exists predicate xs =
    case xs of
          [] => false
        | x::xs' => predicate x orelse exists predicate xs'

val no = exists (fn x => x=7) [4, 11, 23]
val has_zero = exists (fn x => x=0)

(* std library uses partial_application in a lot stuff *)
val increment_all = List.map (fn x => x + 1)
val removeZeros = List.filter (fn x => x <> 0)

(* type vars mystic errors I kept getting are now clear *)
(* issues when returning a polymorphic function in SML, below won't work *)
val pair_with_one = List.map (fn x => (x, 1))

(* workarounds *)
val pair_with_one: string list -> (string * int) list = List.map (fn x => (x,1))

