
fun max1 (xs: int list) =
    if null xs
    then NONE
    else 
        let val tl_ans = max1 (tl xs)
        in 
            (* We need valof because we are expecting a option NONE value at some point in the recursive call *)
            if isSome tl_ans andalso valOf tl_ans > hd xs
            then tl_ans
            else SOME (hd xs)
        end;

(* We need to use valOf since we get back a option which doesn't type check against an integer *)
valOf(max1([3,7,4])) + 2;
