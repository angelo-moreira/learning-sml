
fun sum_list (xs: int list) =
    if null xs
    then 0
    else hd xs + sum_list(tl xs)

(* Multiply all elements inside a list *)
fun list_product (xs: int list) =
    if null xs
    then 1
    else hd xs * list_product (tl xs)

(* Count number -1 and return a list of ints *)
fun countdown (x: int) = 
    if x = 1
    then []
    else x::countdown (x-1);
countdown(7);

fun append (xs: int list, ys: int list) =
    if null xs
    then ys
    else (hd xs)::append((tl xs), ys);

(* My solution looked looked like this and i don't understand why it returned [2,1,3,4] *)
    (* else append( (tl xs), ((hd xs)::ys) ) *)

append ([1,2], [3,4]);

(* functions over pairs of lists *)

fun sum_pair_list (xs: (int*int) list) =
    if null xs
    then 0
    else #1 (hd xs) + #2 (hd xs) + sum_pair_list(tl xs);

sum_pair_list([(1,2),(3,4)]);

fun firsts (xs: (int*int) list) =
    if null xs
    then []
    else (#1 (hd xs))::firsts(tl xs);

fun seconds (xs: (int*int) list) =
    if null xs
    then []
    else (#2 (hd xs))::seconds(tl xs);

(* example of using previous functions to do "composition" *)
fun sum_pair_list2 (xs: (int*int) list) =
    (sum_list (firsts xs) ) + (sum_list (seconds xs) ) ;

