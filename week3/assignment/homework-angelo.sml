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

val full_name = {first="Fred", middle="W", last="Smith"}

fun similar_names (words: string list list, name: {first: string, middle: string, last: string}) =
    let
        fun convert_names (names_to_convert: string list, { middle: string, last: string }) =
            case names_to_convert of
                [] => []
                | to_convert_hd::to_convert_tl => [{ first=to_convert_hd, last=last, middle=middle }]@convert_names (to_convert_tl, { middle=middle, last=last })
    in
        case name of
            { first, middle, last } => [{ first=first, middle=middle, last=last }]@convert_names (get_substitutions2 (words, first), { middle=middle, last=last })
    end




(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black | Green
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color ((s: suit, _)) =
    case s of
        (Clubs | Spades) => Black
        | _ => Red

fun card_value ((_, r:rank)) =
    case r of
        Ace => 11
        | (King | Queen | Jack) => 10
        | (Num i) => i

val cards_sample = [(Hearts, Ace), (Spades, Num 3), (Clubs, Queen)];

fun remove_card (cs: card list, c: card, e) =
    case cs of
        cs::cs' => (case cs=c of true => remove_card (cs', c, e) | false => cs::remove_card (cs', c, e))
        | _ => cs

fun all_same_color (cs: card list) =
    case cs of
        [] => true
        | _::[] => true
        | head::(neck::rest) => card_color (head) = card_color (neck)
                                andalso all_same_color (neck::rest)

fun sum_cards (cs: card list) =
    let
        fun sum_cards_helper (cs: card list, acc: int) =
            case cs of
                [] => acc
                | c::cs' => sum_cards_helper (cs', acc + card_value(c))
    in
        sum_cards_helper (cs, 0)
    end

fun preliminary_score (sum: int, goal: int) =
    case (sum > goal) of
        true => 3 * (sum - goal)
        | false => (goal - sum)

fun score (cs: card list, goal: int) =
    case all_same_color (cs) of
        true => preliminary_score (sum_cards (cs), goal) div 2
        | false => preliminary_score (sum_cards (cs), goal)

fun discard_card (needle: card, cs: card list, e) =
    case cs of
        (c::cs') => (case c = needle of
                        true => cs'
                        | false => c::discard_card (needle, cs', e))
        | _ => raise e

fun officiate (cs: card list, ms: move list, goal: int) =
    let
        fun state_of_game (cs: card list, ms: move list, g: int, cards_held: card list) =
            case (ms, cs, sum_cards(cards_held) > g) of
                (_, c::cs, true) => score (cards_held, g)
                | ( ((Discard i)::ms'), _, _) => state_of_game (cs, ms', goal, discard_card(i, cards_held, IllegalMove))
                | ((m::ms'), (c::cs'), _) => state_of_game (cs', ms', goal, cards_held@[c])
                | _ => score (cards_held, g)
    in
        state_of_game (cs, ms, goal, [])
    end
