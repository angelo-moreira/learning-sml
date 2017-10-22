
fun get_food (food) = 
    case food of
        Name n => 1
        | Date d1 => 2
        | Price p => 3
        | Pizza => 4
        | Ingredients (ingredients) => 5;

(* The return type must be the same otherwise we get a type error *)

