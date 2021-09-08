
% ====== General ======


:- [tests].
:- use_module(library(clpfd)).
:- set_prolog_flag(clpfd_monotonic, true).

clear :- write('\33\[2J').






% ============= Snake =============


snake(RowClues, ColClues, Grid, NewGrid)
    :- copyGrid(Grid,NewGrid),
    checkRowClues(NewGrid,RowClues),
    checkColClues(NewGrid,ColClues),
    countNeighbours(NewGrid),
    snakeConnected(NewGrid).
  

% ======== Predefined ========

copyGrid([],[]) :- !.
copyGrid([Row|G],[RowS|S]) :- copyRow(Row,RowS), copyGrid(G,S).

copyRow([],[]) :- !.
copyRow([-1|R],[_|S]) :- copyRow(R,S).
copyRow([Clue|R],[Clue|S]) :- copyRow(R,S).
    
% ======== Mr. Soldier ========

sumRow([0|Tail], Sum):- sumRow(Tail, S),Sum is S.
sumRow([2|Tail], Sum):- sumRow(Tail, S),Sum is (S + 1).
sumRow([],0).
sumRow([1|Tail], Sum):- sumRow(Tail, S),Sum is (S + 1).

checkRowClues([],[]).
checkRowClues([Row|Grid],[N|Rest]) :-  sumRow(Row,Sum),(Sum == N ; N == -1), checkRowClues(Grid, Rest).

checkColClues(Grid, ColClues):- transpose(Grid, Trans),checkRowClues(Trans, ColClues).

countParts([],0).
countParts([Row|Tail], Sum):- countParts(Tail, Sum1), sumRow(Row,Sum2), Sum is Sum1+Sum2.

at(Grid,X,Y,T):- nth0(Y,Grid,Temp), nth0(X,Temp,T).


walk(_, X, Y, _, _, X, Y, 0, 0).
walk(_, X, Y, _, _, X, Y, 2, 0).
walk(_, X, Y, _, _, X, Y, 1, 0).
walk(_, _, _, _, _, _, _, 0, 0).
walk(_, _, _, _, _, 1, 1).
walk(Grid, X, Y, Xprev, Yprev, _, _, _, Count):-
    Xp is X + 1,
    Yp is Y + 1,
    Xm is X - 1,
    Ym is Y - 1,
    at(Grid, Xp, Y, Type1),
    at(Grid, X, Yp, Type2),
    at(Grid, Xm, Y, Type3),
    at(Grid, X, Ym, Type4),
    walk(Grid, Xp, Y, X, Y,Xprev, Yprev, Type1, CountN),
    walk(Grid, X, Yp, X, Y,Xprev, Yprev, Type2, CountS),
    walk(Grid, Xm, Y, X, Y,Xprev, Yprev, Type3, CountW),
    walk(Grid, X, Ym, X, Y,Xprev, Yprev, Type4, CountE),
    Count is CountN + CountW + CountE + CountS + 1,
    !.

walkSetup(Grid,X):-
    extend_grid(Grid,NewGrid),
    index(NewGrid,Ypos1,Xpos1,1),
    Xpos is Xpos1 - 1,
    Ypos is Ypos1 - 1,
    walk(NewGrid,Xpos,Ypos, Xpos,Ypos,Xpos,Ypos, 3, X).

index(Matrix, Row, Col, Value):-
    nth1(Row, Matrix, MatrixRow),
    nth1(Col, MatrixRow, Value).

snakeConnected(Grid):-
    walkSetup(Grid,Count1),
    countParts(Grid, Count2),
    Count1 == Count2.



% ======== countNeighbours ========


    % - Extends the grid and checks all of the pieces.
    countNeighbours(Grid) :- extend_grid(Grid,NewGrid), pass_rows(NewGrid).


    % - Passes all rows onto the indidual piece checker.
    pass_rows([_,_]):- !.
    pass_rows([R1,R2,R3|Rest]) :- 
    	pass_pieces(R1,R2,R3),
    	append([R2|[R3]],Rest,New),
    	pass_rows(New).


    % - Passes all the individual pieces to the actual checker.
    pass_pieces([_,_],[_,_],[_,_]):- !.
    pass_pieces([_,N,T3|L1],[W,O,E|L2],[_,S,B3|L3]) :- 
    	check_pieces(N,W,O,E,S),
    	append([N|[T3]],L1,N1),
    	append([O|[E]],L2,N2),
    	append([S|[B3]],L3,N3),
    	pass_pieces(N1,N2,N3),
    	(\+nonTouching(O,E,S,B3)).




    % - Checks all the individual pieces for legal patterns.
    
    check_pieces(_,_,0,_,_).
    check_pieces(N,W,Piece,E,S) :- 1 #=< Piece,
	    counts_cell(N,X1),
	    counts_cell(E,X2),
	    counts_cell(S,X3),
	    counts_cell(W,X4),
	    Piece #= X1+X2+X3+X4.



    % === Helpers ===

    isEmpty([]).
    extend_row(OldRow,NewRow) :- append([0|OldRow],[0],NewRow).
    extend_grid_rows([X|Xs],[N]) :- extend_row(X,N), isEmpty(Xs), !.
    extend_grid_rows([X|Xs],NewRows) :- extend_row(X,N), extend_grid_rows(Xs,Ns), append([N],Ns,NewRows).
    extend_grid(OldGrid,NewGrid) :- transpose(OldGrid,TransGrid), extend_grid_rows(TransGrid,RowTransGrid), transpose(RowTransGrid,RowGrid), extend_grid_rows(RowGrid,NewGrid).

    counts_cell(0,0).
    counts_cell(2,1).
    counts_cell(1,1).


% ============ Non touching =============
   

	nonTouching(0,0,0,0) :- false.
    nonTouching(O,E,S,B3) :- 
	    counts_cell(O,X1),
	    counts_cell(E,X2),
	    counts_cell(S,X3),
	    counts_cell(B3,X4),

	    (
	    	(X1 > 0, X2 = 0, X3 = 0,  X4 > 0,!);
	    	(X1 = 0, X2 > 0, X3 > 0,  X4 = 0,!);
	    	(X1 > 0, X2 > 0, X3 > 0, X4 > 0,!)
	    ).
