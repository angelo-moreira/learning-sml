(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "homework-angelo.sml";

val test1 = all_except_option ("string", ["string"]) = SOME []

val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []

val test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []

val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test5 = card_color (Clubs, Num 2) = Black

val test6 = card_value (Clubs, Num 2) = 2

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []

val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

(* val invalid_drawing = officiate ([(Clubs, Ace),(Clubs, Num 8),(Diamonds, King)], [Draw, Discard (Spades, Ace) ,Draw, Draw], 20) = IllegalMove *)
val valid_drawing = officiate ([(Clubs, Ace),(Clubs, Num 8),(Diamonds, King)], [Draw, Draw, Discard (Clubs, Ace) ,Draw], 20) = 2

val passed_goal = officiate ([(Clubs, Ace),(Clubs, Num 8),(Diamonds, King)], [Draw, Draw, Draw], 20) = 27
val passed_goal_but_same_colour = officiate ([(Clubs, Ace),(Clubs, Num 8),(Spades, King)], [Draw, Draw, Draw], 20) = 13

val before_goal = officiate ([(Clubs, Num 2),(Clubs, Num 4),(Spades, Num 8), (Hearts, Num 4)], [Draw, Draw, Draw, Draw], 20) = 2
val short_cards = officiate ([(Clubs, Num 2),(Diamonds, Num 4),(Spades, Num 8)], [Draw, Draw, Draw, Draw], 20) = 6


(* val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
             
              *)
