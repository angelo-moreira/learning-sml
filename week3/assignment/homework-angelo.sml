(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (to_remove: string, xs) =
    let fun remove_string (to_remove, xs) =
        case xs of
            [] => []
            | xs::xs' => (case same_string (to_remove, xs) of
                            true => remove_string (to_remove, xs')
                            | false => [xs]@remove_string (to_remove, xs'))
    in 
        case xs of
            [] => NONE
            | xs::xs' => SOME (remove_string (to_remove, (xs::xs')))
    end

val a = [["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]]
val b = ["Freddie","Fred","F"]

fun get_substitutions1 (list_words: string list list, s: string) =
    case list_words of
        [] => []
        | list_words::rest => case all_except_option (s, list_words) of
                                SOME words => case words = list_words of
                                                true => get_substitutions1(rest, s)
                                                | false => words@get_substitutions1(rest, s)

fun get_substitutions2 (list_words: string list list, s: string) =
    let
        fun get_words (list_words: string list list, s, acc_matches: string list) =
            case list_words of
                [] => acc_matches
                | words::rest =>
                    case all_except_option (s, words) of
                        SOME matches => 
                            (case matches = words of
                                true => get_words (rest, s, acc_matches)
                                | false => get_words (rest, s, acc_matches@matches))
    in
        case list_words of
            [] => []
            | list_words => get_words (list_words, s, [])
    end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
