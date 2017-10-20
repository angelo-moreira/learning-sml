(* List of integeres *)
val alist = [4, 5, 6];

(* List of pairs *)
val blist = [(1,2), (3,4), (5,6)];

(* Access head and tail *)
hd(blist);
tl(blist);

(* Add elements to the list *)
(7,8)::blist;

(* You can go deep into all sorts of structures *)
hd (tl (blist));

(* and even access pairs with the #1 notation to get the first element of a pair coooool *)
(#1 (hd (tl (blist) )));

