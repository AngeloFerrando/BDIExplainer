// mars robot 1
// MW: modified to be more idiomatic Jason(!) and with some tracing 

/* Initial beliefs */
at(P) :- pos(P,X,Y) & pos(r1,X,Y).

/* Initial goal */
!check(slots).

/* Plans */
+!check(slots) : not garbage(r1) <- .print(check_slots) ; next(slot); !check(slots).
// MW added plan:
+!check(slots) : garbage(r1) <- .print(check_slots2) ; !carry_to(r2) ; !check(slots).

+!carry_to(R)
   <- .print(carry_to(R)) ; 
    ?pos(r1,X,Y); -+pos(last,X,Y); // remember where to go back
	!take(garb,R); // carry garbage to r2
	!at(last).  // goes back 

+!take(S,L) <- .print(take(S,L)) ; !ensure_pick(S); !at(L); drop(S).

+!ensure_pick(S) : garbage(r1) <- .print(ensure_pick(S)) ; pick(garb); !ensure_pick(S).
+!ensure_pick(_) <- .print(ensure_pick2).

+!at(L) : at(L) <- .print(at1(L)).
+!at(L) <- .print(at2(L)) ; ?pos(L,X,Y); move_towards(X,Y); !at(L).
