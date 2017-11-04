(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (to_remove: string, xs) =
    let 
        fun remove_string (to_remove, xs) =
            case xs of
                x::xs' => (case same_string (to_remove, x) of
                                true => remove_string (to_remove, xs')
                                | false => [x]@remove_string (to_remove, xs'))
                | []  => []
        val new_list = remove_string (to_remove, xs);
    in 
        case xs = new_list of
            true => NONE
            | false => SOME (new_list)
    end

fun get_substitutions1 (ws: string list list, s: string) =
    case ws of
        w::ws' => (case all_except_option (s, w) of
                    SOME words => words@get_substitutions1(ws', s)
                    | NONE => get_substitutions1(ws', s))
        | _ => []

fun get_substitutions2 (ws: string list list, s: string) =
    let
        fun get_words (ws: string list list, s: string, acc: string list) =
            case ws of
                w::ws' => (case all_except_option (s, w) of
                            SOME words => get_words (ws', s, acc@words)
                            | NONE => get_words (ws', s, acc))
                | _ => acc
    in get_words (ws, s, []) end

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

datatype color = Red | Black
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

fun remove_card (cs: card list, c: card, e) =
    case cs of
        (c'::cs') => (case c' = c of
                        true => cs'
                        | false => c::remove_card (cs', c, e))
        | _ => raise e

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

fun officiate (cs: card list, ms: move list, goal: int) =
    let
        fun state_of_game (cs: card list, ms: move list, g: int, cards_held: card list) =
            case (ms, cs, sum_cards(cards_held) > g) of
                (_, c::cs, true) => score (cards_held, g)
                | ( ((Discard i)::ms'), _, _) => state_of_game (cs, ms', goal, remove_card(cards_held, i, IllegalMove))
                | ((m::ms'), (c::cs'), _) => state_of_game (cs', ms', goal, cards_held@[c])
                | _ => score (cards_held, g)
    in state_of_game (cs, ms, goal, []) end
