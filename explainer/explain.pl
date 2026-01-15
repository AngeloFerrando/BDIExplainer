/* 
Code to generate explanations for Jason (Michael Winikoff) 

Usage: 
% swipl -q -t go explain.pl > output.pl
OR: (interactive)
% swipl
...
| ?- [explain].  
...
| ?- go.

For interactive mode some factors are of the form type(Content,More) - where More is the thing to query for more explanatory factors.

History:
30/10/2025 initial version (v0.1)
12/11/2025: added pretty-printing of plans, and extended getcoffee -> gethb (hot beverage)
13/11/2025: added some more documentation
17-19/11/2025: ported to Jason (see explainer.asl), added "interactive" switch  and simplified, 
27/11/2025: Decided to stick with Prolog, and adjusted for SWI-Prolog (nth -> nth1)
2/12/2025: Modified to relax assumptions and added another example. Also added top-level go predicate to run all examples.
	(a) Can now handle loops (made plan_contains and e(...) loop-resistant); also adjusted e(...) to check for conditions holding, to avoid incorrect factors when using the wrong plan due to a loop
	(b) Extended trace and held to include a time stamp (*)
Note: we assume that an action or goal can only occur in a single plan. Can rename to ensure, or could relax by adding additional tags or tracing for what is called from where.
(*) It's actually a "reasoning identifier": all the reasoning relating to a given action needs to be given the same time stamp

TODO:
- BUG: if conditions for multiple plans hold then recursive plans can give extra spurious val(L1<L2)
- Consider whether to take into account plan order, i.e. SeqOne, not One - DONE:
  Using SeqOne makes contrastive explanations much less useful, and does not use valuings 
- Extend to propensities
- Check what we can get from Jason (using ChatBDI) and extend to get anything else that is needed (waiting on Andrea)
*/

% run all queries, both interactive and non-interactive
go :- 
	write('*** Running queries in interactive mode ... ***'), nl, nl, 
	new_trace([ (1,act(getOwnCard)), (2,act(goto(kitchen))), (3,act(getCoffee(kitchen))) ]),
	make_interactive(yes),
	e1, nl, e2, nl, ec1, nl, ec3, nl, ec4, nl, ec5, nl, 
	nl, write('*** Running queries in non-interactive mode ... ***'), nl, nl, 
	make_interactive(no),
	e1, nl, e2, nl, ec1, nl, ec3, nl, ec4, nl, ec5, nl, nl,
	write('*** Running recursive program in interactive mode ... ***'), nl, nl, 
	new_trace([ (1,act(eatchocolate)), (2,act(eatchocolate)), (3,act(drinkcoffee)), (4,act(smile)) ]),
	make_interactive(yes), ecs, nl,
	write('*** Running recursive program in non-interactive mode ... ***'), nl, nl, 
	make_interactive(no), ecs, nl, nl.

:- dynamic interactive/1.
interactive(yes). % configure yes or no
make_interactive(X) :- retractall(interactive(_)), assert(interactive(X)).

% Example queries: 
e1 :- explain(act(getOwnCard),1,null).
e2 :- explain(goal(getStaffCard),1,null).
ec1 :- explain(act(getOwnCard),1,act(getOthersCard)).
ec11 :- explain(act(getOthersCard),1,act(getOwnCard)).
ec2 :- explain(act(getOwnCard),1,act(goto(office))).
ec3 :- explain(goal(getStaffCard),1,act(goto(shop))).
ec4 :- explain(goal(getKitchenCoffee),1,goal(getShopCoffee)).
ec5 :- explain(goal(getKitchenCoffee),1,null).
ecs :- 
		explain(act(eatchocolate),1,null), nl,
%		explain(act(eatchocolate),1,act(drinkcoffee)), nl,
%		explain(act(eatchocolate),1,act(smile)), nl,
		explain(act(eatchocolate),2,null), nl,
%		explain(act(eatchocolate),2,act(drinkcoffee)), nl,
%		explain(act(eatchocolate),2,act(smile)), nl,
		explain(act(drinkcoffee),3,null), nl,
%		explain(act(drinkcoffee),3,act(eatchocolate)), nl,
%		explain(act(drinkcoffee),3,act(smile)), nl,
		explain(act(smile),4,null), nl,
%		explain(act(smile),4,act(eatchocolate)), nl,
%		explain(act(smile),4,act(drinkcoffee)), nl,
		nl.

% top level call
explain(X,TS,F) :- 
	write('Why '), write(X), write('@'), write(TS), (F=null -> true ; (write(' and not '), write(F))),  write(' ?'), nl, 
	(setof(E,e(X,TS,F,E),Es) -> px(Es) ; (write('=> no explanatory factors!'),nl)).

% print explanation
px([]).
px([E|Es]) :- ((E =.. [V1,V2,More]) -> (E1 =.. [V1,V2], write(E1), write(' for more, ask why '), write(More), nl) ; (write(E), nl)), px(Es).

e(X,TS,Ex) :- e(X,TS,null,[X],Ex).
e(X,TS,F,Ex) :- e(X,TS,F,[X],Ex).

% These are the key rules for explanatory factors; 
% 2/12: adjusted with timestamps TS and added track of already asked Xs to avoid loops
e(X,TS,F,Xs,Ex) :- p(L,T,_C,B), member(X,B), \+ plan_contains(L,F,_), 
	(interactive(yes) -> Ex=desire(L,T) ; (Ex=desire(L);e_aux(T,TS,F,Xs,Ex))).
e(X,TS,_,_,cond(Pre)) :- tr(Tr), member((TS,X),Tr), prepost(X,Pre1,_), 
	prior_posts(X,Posts), subtract(Pre1,Posts,Pre), Pre\=[].
% Changed member(X,B) to add case for plan for the top-level goal (26/11)
% This case should check that the condition held (error if it doesn't)
e(X,TS,F,Xs,Ex) :- p(L,T,C,B), (member(X,B);X=goal(L)), \+ plan_contains(L,F,_), held(C,TS),
	(interactive(yes) -> Ex=ccond(C,T) ; (Ex=ccond(C);e_aux(T,TS,F,Xs,Ex))).
% For contrastive should only consider foil if it's provided (next two plans) (26/11)
% Comment out next two clauses if want to specify SeqOne
e(X,TS,F,Xs,Ex) :- p(L1,T,_,B), (member(X,B);X=goal(L1)), p(L2,T,C,_), L1\=L2, \+held(L2,TS), held(L1,TS),
	\+ plan_contains(L1,F,_), (F=null ; F=goal(L2) ; plan_contains(L2,F,_)),
	(interactive(yes) -> Ex=ncond(C,T) ; (Ex=ncond(C);e_aux(T,TS,F,Xs,Ex))).
e(X,TS,F,Xs,Ex) :- p(L1,T,_,B), (member(X,B);X=goal(L1)), p(L2,T,_,_), L1\=L2, held(L2,TS), held(L1,TS),
	\+ plan_contains(L1,F,_), (F=null ; F=goal(L2) ; plan_contains(L2,F,_)),
	(interactive(yes) -> Ex=val(L2<L1,T) ; (Ex=val(L2<L1);e_aux(T,TS,F,Xs,Ex))).
% Alternative for seq-OR - note that if we use Seq-OR then we don't use valuings and we don't need held(_), but do need plan order XXX
% If we limit to only look at N_f then the extra sequencing condition is redundant
/*
e(X,TS,F,Xs,Ex) :- p(L1,T,_,B), (member(X,B);X=goal(L1)), p(L2,T,C,_), L1\=L2, 
	\+ plan_contains(L1,F,_), (F=null ; F=goal(L2) ; plan_contains(L2,F,_)),
	plan_num(L1,N1), plan_num(L2,N2), N2<N1, 
	(interactive(yes) -> Ex=ncond(C,T) ; (Ex=ncond(C);e_aux(T,TS,F,Xs,Ex))).
*/
e_aux(X,TS,F,Xs,Ex) :- \+ member(X,Xs), e(X,TS,F,[X|Xs],Ex).

% This does filtering out of pre-conditions that are achieved by post conditions of prior actions
prior_posts(X,Posts) :- ssetof(C,prior_post(X,C),Posts).
prior_post(X,C) :- seqBef(N,X), N=act(_), tr(Tr), member(N,Tr), prepost(N,_,Post), member(C,Post).
seqBef(S1,S2) :- plan_contains(L,S1,St1), plan_contains(L,S2,St2), p(L,_,_,Body), nth1(N1,Body,St1), nth1(N2,Body,St2), N1<N2.

% Does a plan contain a given step S? (third argument is the child of L that
% contains S, which may be S itself, or a goal that contains S below it in the
% goal-plan tree)
% 2/12: now uses auxiliary predicate to avoid loops
plan_contains(L,S,S) :- p(L,_,_,B), member(S,B).
plan_contains(L,S,G) :- p(L,_,_,B), member(G,B), G=goal(_), p(L2,G,_,_), 
	plan_contains_aux(L2,[L],S).
% plan_contains_aux(Label:in,ListOfLabelSeen:in,Step:out)
plan_contains_aux(L,_Ls,S) :- p(L,_,_,B), member(S,B).
plan_contains_aux(L,Ls,S) :- p(L,_,_,B), \+ member(L,Ls), member(G,B), G=goal(_), p(L2,G,_,_), plan_contains_aux(L2,[L|Ls],S).

% setof, but return [] if no solutions rather than failing 
ssetof(A,B,C) :- (setof(A,B,C) -> true ; C=[]).

/* Coffee Example 
   Define: tr(Tr), p(L,T,C,B), prepost(act(A), Pre, Post) and held(L)
   Note: trigger given as goal(...) 
   Plan steps are either act(action) or goal(sub-goal), presented as a list.
   We indicate beliefs as held(Label) if plan's context held when it was considered
*/

:- dynamic tr/1.
% tr: trace of actions done; 2/12: added timestamp 
tr([ (1,act(getOwnCard)), (2,act(goto(kitchen))), (3,act(getCoffee(kitchen))) ]).

% p = plan library: p(Label, Trigger, Context, Body)
p(getTea, goal(gethb), wantTea, 
	[act(goto(kitchen)), act(getTea) ]).
p(getKitchenCoffee, goal(gethb), staffCardAvailable, 
	[goal(getStaffCard), act(goto(kitchen)), act(getCoffee(kitchen)) ]).
p(getOfficeCoffee, goal(gethb), annInOffice, 
	[act(goto(office)), act(getPod), act(getCoffee(office)) ]).
p(getShopCoffee, goal(gethb), haveMoney,
	[act(goto(shop)), act(pay(shop)), act(getCoffee(shop)) ]).
p(getOwnCard, goal(getStaffCard), ownCard, [act(getOwnCard)]).
p(getOthersCard, goal(getStaffCard), colleagueAvailable, [act(getOthersCard)]).

% Some more plans to test loops
p(behappy1,goal(behappy),happy, [act(smile)]).
p(behappy2,goal(behappy),havechocolate, [act(eatchocolate),goal(behappy)]).
p(behappy3,goal(behappy),havecoffee, [act(drinkcoffee),goal(behappy)]).

new_trace(T) :- retractall(tr(_)), assert(tr(T)).

:- discontiguous held/2.

held(behappy2,1).
held(behappy2,2).
held(behappy3,2).
held(behappy3,3).
held(behappy1,4).
held(behappy2,4).

% NEW: plan order - could be captured in a better way (currently not used)
plan_num(getTea,1).
plan_num(getKitchenCoffee,2).
plan_num(getOfficeCoffee,3).
plan_num(getShopCoffee,4).
plan_num(getOwnCard,1).
plan_num(getOthersCard,2).

% Not currently used 
% plan_before(getKitchenCoffee,getOfficeCoffee).
% plan_before(getOfficeCoffee,getShopCoffee).
% plan_before(getKitchenCoffee,getShopCoffee).
% plan_before(getOwnCard,getOthersCard).

% pre and post conditions for actions: prepost(action, pre, post) 
% Note: if don't have post-conditions then will just get unfiltered pre-conditions
% If don't have pre-conditions then will just be missing those from the explanations
prepost(act(getOwnCard), [ownCard], [haveCard]).
prepost(act(getOthersCard), [colleagueAvailable], [haveCard]).
prepost(act(goto(kitchen)), [], [at(kitchen)]).
prepost(act(getCoffee(kitchen)), [at(kitchen), haveCard], [haveCoffee]).
prepost(act(goto(office)), [annInOffice], [at(office)]).
prepost(act(getPod), [], [havePod]).
prepost(act(getCoffee(office)), [at(office), havePod], [haveCoffee]).
prepost(act(goto(shop)), [], [at(shop)]).
prepost(act(pay(shop)), [haveMoney], [paidForCoffee]).
prepost(act(getCoffee(shop)), [at(shop), paidForCoffee], [haveCoffee]).

% held captures for which plans did their context condition hold when they were being considered
% 2/12: added timestamp
held(getKitchenCoffee,1).
held(getOthersCard,1).
held(getTea,1).

% Pretty-print the plans above
pp :- p(L,T,C,B), format("@~k:~n~k : ~k <- ",[L,T,C]), pp(B), fail.
pp.
pp([B]) :- pps(B), write('.'), nl.
pp([B,B2|Bs]) :- pps(B), write('; '), pp([B2|Bs]).
pps(goal(X)) :- format("!~w",[X]).
pps(act(X)) :- format("~w",[X]).

