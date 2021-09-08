% ========== GENERAL ==========
clear :- write('\33\[2J').


% ========== Series 1 ==========

% nah, have done most of dis alr, ima skip fam


% ========== Series 2 ========== 


% ---- Predicates ----
position(atdoor).
position(atwindow).
position(middle).

level(onfloor).
level(onbox).

banana(has).
banana(hasnot).



% ---- Rules ----
goal(state(_,_,_,has)).

init(state(atdoor,onfloor,atwindow,hasnot)).

% state(P1,L,P2,H) :- position(P1),level(L),position(P2),banana(H).

move(state(middle,onbox,middle,hasnot), grab, state(middle,onbox,middle,has)).
move(state(P,onfloor,P,H), climb, state(P,onbox,P,H)).
move(state(P1,onfloor,P1,H), push(P1,P2), state(P2,onfloor,P2,H)). % :- position(P2).
move(state(P1,onfloor,B,H), walk(P1,P2), state(P2,onfloor,B,H)). % :- position(P2). 

solve(S) :- goal(S).
solve(S1) :- move(S1,_,S2),solve(S2).

solvable :- init(S),solve(S).