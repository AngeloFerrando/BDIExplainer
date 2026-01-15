/* 
Cleaning robot example v2
This version uses a goal-plan instance tree: expanded and grounded. This avoids the issues with uninstantiated variables and loops.
The encoding is then: (L=label, G=goal, C=context condition, B=body, A=action, TS=time step)
- p(TS,L,G,C,B) - need to add numbers to disambiguate, since each plan instantiated has a new copy
	B = sequence of (TS,act(A)), (TS,goal(G)), test(C), add_bel(B), rem_bal(B)
- tr([(TS,A),...])
- plan_num(L,Num)
- prepost(TS,A,Pre,Post) 
*/

go :- write('Explaining all actions in trace:\n\n'), tr(Tr), member((N,A),Tr), once(explain(A,N,null)), nl, fail.
go :- write('\nDone.').

% tr: trace of actions done: ran the Jason example with a simplified grid (3x3, with garbage at two locations in the top row)
tr([
(1,act(next(slot))),
(2,act(pick(garb))),
(3,act(move_towards(1,1))),
(4,act(drop(garb))),
(5,act(move_towards(1,0))),
(6,act(next(slot))),
(7,act(pick(garb))),
(8,act(move_towards(1,1))),
(9,act(drop(garb))),
(10,act(move_towards(2,0))),
(11,act(next(slot))),
(12,act(next(slot))),
(13,act(next(slot))),
(14,act(next(slot))),
(15,act(next(slot))),
(16,act(next(slot)))
]).


% MW: the following captures the order of plans for each trigger, and is used in the sequential or explanation code
plan_num(checkslot_rec, 1).
plan_num(checkslot_base, 2).
plan_num(ensure_pick_rec,1).
plan_num(ensure_pick_base,2).
plan_num(at_base,1).
plan_num(at_rec,2).
% Next redundant?
plan_num(garbage,1).
plan_num(carry_to,1).
plan_num(take,1).

% p = plan library: p(TimeStamp, Label, Trigger, Context, Body)

% @checkslot_rec +!check(slots) : not garbage(r1) <- next(slot); !check(slots).
p(17,checkslot_done,goal(check(slots)),not(garbage(r1)), []).
p(TS,checkslot_rec, 
   goal(check(slots)),  
   not(garbage(r1)), 
   [(TS,act(next(slot))), (TS1,goal(check(slots)))]) :- TS1 is TS+1, TS<17,
	tr(TR), member((TS,act(next(slot))),TR). % only applies when the nextslot is done

% +!check(slots) : garbage(r1) <- .print(check_slots2) ; !carry_to(r2) ; !check(slots).
p(TS,checkslot_base,
	goal(check(slots)),
	garbage(r1),
	[(TS,goal(carry_to(r2))), (TS4,goal(check(slots)))]
) :- TS4 is TS+4, tr(TR), member((TS,act(pick(garb))),TR).
% This is hardwired 
	

/*
@carry_to +!carry_to(R) <-
      ?pos(r1,X,Y); -+pos(last,X,Y); // remember where to go back
      !take(garb,R); // carry garbage to r2
      !at(last). // goes back before continuing
*/
p(TS,carry_to,
   goal(carry_to(R)),
   true,
   [test(pos(r1,X,Y)), rem_bel(pos(last,_,_)), add_bel(pos(last,X,Y)), (TS,goal(take(garb,R))), (TS3,goal(at(last)))]
) :- R=r2, TS3 is TS+3, tr(TR), member((TS,act(pick(garb))),TR).

% @take +!take(S,L) : true <- !ensure_pick(S); !at(L); drop(S).
p(TS,take,
   goal(take(S,L)),
   true,
   [(TS,goal(ensure_pick(S))), (TS1,goal(at(L))), (TS2,act(drop(S)))]
) :- S=garb, L=r2, TS1 is TS+1, TS2 is TS+2, tr(TR), member((TS,act(pick(garb))),TR).

% @ensure_pick_rec +!ensure_pick(S) : garbage(r1) <- pick(garb); !ensure_pick(S).
p(TS,ensure_pick_rec,
   goal(ensure_pick(S)),
   garbage(r1),
   [(TS,act(pick(garb))), (TS1,goal(ensure_pick(S)))]
) :- TS1 is TS+1, tr(TR), member((TS,act(pick(garb))),TR).

% @ensure_pick_base +!ensure_pick(_).
p(TS,ensure_pick_base,
   goal(ensure_pick(_)),
   true,
   []
) :- tr(TR), member((TS,act(move_towards(1,1))),TR).

% @at_base +!at(L) : at(L).
p(TS,at_base,
   goal(at(L)),
at(L),
   []
) :- tr(TR), TS1 is TS-1, member((TS1,act(move_towards(_,_))),TR), 
	(
	(member((TS,act(drop(garb))),TR),L=r2) ; 
	(member((TS,act(next(slot))),TR), L=last)
	).

% @at_rec +!at(L) <- ?pos(L,X,Y); move_towards(X,Y); !at(L).
p(TS,at_rec,
   goal(at(L)),
   true,
   [test(pos(L,X,Y)), (TS,act(move_towards(X,Y))), (TS1,goal(at(L)))]
) :- TS1 is TS+1, tr(TR), member((TS,act(move_towards(X,Y))),TR), 
	(Y=1 -> L=r2 ; L=last).

% added definitions for test, add, del pre/post conditions - these are generic and should really go in the engine
prepost(TS,test(C),[C],[]) :- member(TS,[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]).
prepost(TS,add_bel(B),[],[B]) :- member(TS,[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]).
prepost(TS,del_bel(B),[],[not(B)]) :- member(TS,[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]).

/* ACTIONS: move_towards(X,Y), pick(P), drop(S), next(slot) */ 
prepost(TS, act(move_towards(X,Y)), [], [pos(r1,X,Y)]) :-
		member(TS,[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]).
prepost(TS, act(pick(_)), [], []) :-
		member(TS,[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]).
prepost(TS, act(drop(_)), [], []) :-
		member(TS,[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]).
prepost(TS, act(next(slot)), [], []) :-
		member(TS,[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]).
