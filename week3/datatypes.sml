
datatype Food = Name of string
         | Date of int * int * int
         | Price of int
         | Ingredients of string list
         | Pizza

val a = Name "Pizza"
val b = Name
val c = Pizza
val d = Date (20, 10, 2017)
val e = a


datatype suit = Club | Diamond | Heart | Spade
datatype rank = Jack | Queen | King |
                Ace | Num of int

