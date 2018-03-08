(***************************************************************
*
* CSCI 305 - ML Programming Lab
*
* Connor Grace
* connor.grace88@gmail.com
*
***************************************************************)

(* Quit program - OS.Process.exit(OS.Process.success); *)

(* Define your data type and functions here *)
datatype 'element set = Empty
   | Set of 'element * 'element set;

fun f [] = [] (* a if the list is empty, the empty list is returned*)
   | f (x::xs) = (x + 1) :: (f xs); (* b if the list is not empty, 1 is added to every value in the list*)

fun isMember e set =
   if
     set <> Empty
   then
     let
       val (Set s) = set;
     in
       if
         #1(s) <> e
       then
         isMember e (#2(s))
       else
         true
     end
   else
     false;

fun list2Set list set =
   if
     length(list) > 0
   then
     let
       val s = Set(List.last(list), set)
       val l = List.take(list, length(list)-1)
     in
       if
         isMember (List.last(list)) set
       then
         list2Set l set
       else
         list2Set l s
     end
   else
     set;

fun union set1 set2 =
   if
     set1 <> Empty
   then
     let
       val (Set s) = set1
       val i = #1(s)
       val j = #2(s)
       val f = Set(i, set2)
     in
       if
         isMember i set2
       then
         union j set2
       else
         union j f
     end
   else
     set2;

fun intersect set1 set2 set3 =
   if
     set1 <> Empty
   then
     let
       val (Set s) = set1
       val i = #1(s)
       val j = #2(s)
       val f = Set(i, set3)
     in
       if
         isMember i set2
       then
         intersect j set2 (union (Set(i, Empty)) set3)
       else
         intersect j set2 set3
     end
   else
     set3;

(* Simple function to stringify the contents of a Set of characters *)
fun stringifyCharSet Empty = ""
  | stringifyCharSet (Set(y, ys)) = Char.toString(y) ^ " " ^ stringifyCharSet(ys);

(* Simple function to stringify the contents of a Set of ints *)
fun stringifyIntSet Empty = ""
  | stringifyIntSet (Set(w, ws)) = Int.toString(w) ^ " " ^ stringifyIntSet(ws);

(* Simple function to stringify the contents of a Set of strings *)
fun stringifyStringSet Empty = ""
  | stringifyStringSet (Set(z, zs)) = z ^ " " ^ stringifyStringSet(zs);

(* Simple function that prints a set of integers *)
fun print_int x = print ("{ " ^ stringifyIntSet(x) ^ "}\n");

(* Simple function that prints a set of strings *)
fun print_str x = print ("{ " ^ stringifyStringSet(x) ^ "}\n");

(* Simple function that prints a set of characters *)
fun print_chr x = print ("{ " ^ stringifyCharSet(x) ^ "}\n");

list2Set ([1, 3, 2]) (Empty);
list2Set ([#"a", #"b", #"c"]) (Empty);
list2Set ([]) (Empty);
list2Set ([6, 2, 2]) (Empty);
list2Set (["x", "y", "z", "x"]) (Empty);

(* Question 1 *)
f [3, 1, 4, 1, 5, 9];

(* Question 5 *)
val quest5 = isMember ("one") (list2Set (["1", "2", "3", "4"]) (Empty));
print ("\nQuestion 5: " ^ Bool.toString(quest5) ^ "\n");

(* Question 7 *)
val quest7 = list2Set (["it", "was", "the", "best", "of", "times,", "it", "was", "the", "worst", "of", "times"]) (Empty);
print "\nQuestion 7: ";
print_str quest7;
print "\n";

(* Question 9 *)
print "\nQuestion 9: ";
print_str (union (list2Set (["green", "eggs", "and"]) (Empty)) (list2Set (["ham"]) (Empty)));

(* Question 10 *)
print "\nQuestion 10: ";
print_str (intersect (list2Set (["stewed", "tomatoes", "and", "macaroni"]) (Empty)) (list2Set (["macaroni", "and", "cheese"]) (Empty)) (Empty));
