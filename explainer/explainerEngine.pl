/* 
Code to generate explanations for Jason (Michael Winikoff) 
This version uses the new instantiated goal-plan tree representation.

TODO:
- Extend to propensities
- Check what we can get from Jason (using ChatBDI) and extend to get anything else that is needed (waiting on Andrea)
*/

:- discontiguous held/2.

% trace update
new_trace(T) :- retractall(tr(_)), assert(tr(T)).

:- dynamic interactive/1.
interactive(yes). % no or yes

% top level call
explain(X,TS,F) :- 
	write('Why '), write(X), write('@'), write(TS), (F=null -> true ; (write(' and not '), write(F))),  write(' ?'), nl, 
	(setof(E,e(X,TS,F,E),Es) -> px(Es) ; (write('=> no explanatory factors!'),nl)).

% print explanation
px([]).
px([E|Es]) :- ((E =.. [V1,V2,More]) -> (E1 =.. [V1,V2], write(E1), write(' for more, ask why '), More=(TS,T), write(T),write('@'),write(TS), nl) ; (pxwrite(E), nl)), px(Es).
% treat '@'(X,TS) differently when printing 
pxwrite(ccond('@'(X,TS))) :- !, write('cccond '),write(X),write('@'),write(TS).
pxwrite(ncond('@'(X,TS))) :- !, write('ncond '),write(X),write('@'),write(TS).
pxwrite(desire('@'(X,TS))) :- !, write('desire '),write(X),write('@'),write(TS).
pxwrite(E) :- write(E).

e(X,TS,Ex) :- e(X,TS,null,[X],Ex).
e(X,TS,F,Ex) :- e(X,TS,F,[X],Ex).

% These are the key rules for explanatory factors; 
% the plan's TS may be different to the action, but TS must be ground
e(X,TS,F,Xs,Ex) :- member(TS1,[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]), 
	p(TS1,L,T,_C,B), member((TS,X),B), \+ plan_contains(L,F,_), 
	(interactive(yes) -> Ex=desire(L,(TS1,T)) ; (Ex=desire('@'(L,TS1));e_aux(T,TS1,F,Xs,Ex))).
e(X,TS,_,_,cond(Pre)) :- tr(Tr), member((TS,X),Tr), prepost(TS,X,Pre1,_), 
	prior_posts(X,Posts), subtract(Pre1,Posts,Pre), Pre\=[].
% Changed member(X,B) to add case for plan for the top-level goal (26/11)
% This case should check that the condition held (error if it doesn't)
e(X,TS,F,Xs,Ex) :- member(TS1,[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]),
	p(TS1,L,T,C,B), (member((TS,X),B);X=goal(L)), \+ plan_contains(L,F,_), 
	(interactive(yes) -> 
		(C=true -> fail ; Ex=ccond(C,(TS1,T))) ; 
		(C=true -> e_aux(T,TS,F,Xs,Ex) ; (Ex=ccond('@'(C,TS1));e_aux(T,TS1,F,Xs,Ex)))).
% For contrastive should only consider foil if it's provided (next two plans) (26/11)
% seq-OR ...
% If we limit to only look at N_f then the extra sequencing condition is redundant
% This case uses a hack: it assumes that all plans will be used somewhere, 
% so it finds the alternative plans in the plan instances.
% Ideally, we should have the plans and their instances, and for this one 
% use the plans for the non-selected alternatives
e(X,TS,F,Xs,Ex) :- member(TS1,[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]),
	p(TS1,L1,T,_,B), (member((TS,X),B);X=goal(L1)), 
	member(TS2,[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]),
	p(TS2,L2,T,C,_), L1\=L2, 
	\+ plan_contains(L1,F,_), (F=null ; F=goal(L2) ; plan_contains(L2,F,_)),
	plan_num(L1,N1), plan_num(L2,N2), N2<N1, 
	(interactive(yes) -> Ex=ncond(C,(TS1,T)) ; (Ex=ncond('@'(C,TS1));e_aux(T,TS1,F,Xs,Ex))).
% Turn off loop detection 
% e_aux(X,TS,F,Xs,Ex) :- \+ member(X,Xs), e(X,TS,F,[X|Xs],Ex).
e_aux(X,TS,F,Xs,Ex) :- e(X,TS,F,[X|Xs],Ex).

% This does filtering out of pre-conditions that are achieved by post conditions of prior actions
% MW: TODO - update to deal with negations in pre- and post-conditions - but not needed for the example
prior_posts(X,Posts) :- ssetof(C,prior_post(X,C),Posts).
% MW: 6/1 updated to handle trace of (N,A) instead of just A
prior_post(X,C) :- seqBef(A,X), A=act(_), tr(Tr), member((N,A),Tr), prepost(N,A,_,Post), member(C,Post).
seqBef(S1,S2) :- plan_contains(L,S1,St1), plan_contains(L,S2,St2), p(_TS,L,_,_,Body), nth1(N1,Body,St1), nth1(N2,Body,St2), N1<N2.

% Does a plan contain a given step S? (third argument is the child of L that
% contains S, which may be S itself, or a goal that contains S below it in the
% goal-plan tree)
% 2/12: now uses auxiliary predicate to avoid loops
plan_contains(L,S,S) :- member(TS,[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]), 
	p(TS,L,_,_,B), member(S,B).
plan_contains(L,S,G) :- 
	member(TS,[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]),
	p(TS,L,_,_,B), member(G,B), G=(TS1,goal(_)), p(TS1,L2,G,_,_), 
	plan_contains_aux(L2,[L],S).
% plan_contains_aux(Label:in,ListOfLabelSeen:in,Step:out)
plan_contains_aux(L,_Ls,S) :- p(_,L,_,_,B), member(S,B).
plan_contains_aux(L,Ls,S) :- p(_,L,_,_,B), \+ member(L,Ls), member(G,B), G=goal(_), p(L2,G,_,_), plan_contains_aux(L2,[L|Ls],S).

% setof, but return [] if no solutions rather than failing 
ssetof(A,B,C) :- (setof(A,B,C) -> true ; C=[]).

% pt = print tree (from program)
pt :- 
	write('\\begin{forest} for tree={ellipse,draw,l=1cm},'),nl,
	ptg(1,goal(check(slots))),
	nl, write('\\end{forest}'),nl.
ptg(TS,G) :- write('['), 
		p(TS,L,G,C,B),
		write('{'),write(L),
		(C=true -> write('} ') ; (write(':'), write(C), write('} '))),
		ptps(B), write(']').
ptps([]).
ptps([(TS,act(A))|R]) :- !, write('[\\textbf{'), write(TS), write(':'), write(A), write('}] '), ptps(R).
ptps([(TS,goal(G))|R]) :- !, ptg(TS,goal(G)), ptps(R).
ptps([_|R]) :- ptps(R).
