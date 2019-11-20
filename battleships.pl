
% author - Michal Pocatko
% last edit - 22.6.2017

% solve(?Grid, +Horizontal, +Vertical, +Ships)
solve(Grid,Horizontal,Vertical,Ships):-
	checkGrid(Grid),
	checkHorizontal(Grid,Horizontal),
	checkVertical(Grid,Vertical),
	checkShips(Grid,Ships),
	printGrid(Grid),!.

ship(u). %end of ship facing up
ship(d). %end of ship facing down
ship(l). %end of ship facing left
ship(r). %end of ship facing right
ship(c). %center ship piece
ship(s). %single size ship

% all the possible horizontal combinations of cells
hCombination(u,w).
hCombination(d,w).
hCombination(l,r).
hCombination(l,c).
hCombination(r,w).
hCombination(c,r).
hCombination(c,c).
hCombination(c,w).
hCombination(s,w).
hCombination(w,u).
hCombination(w,d).
hCombination(w,l).
hCombination(w,c).
hCombination(w,s).


% all the possible vertical combinations of cells
vCombination(u,d).
vCombination(u,c).
vCombination(d,w).
vCombination(l,w).
vCombination(r,w).
vCombination(c,d).
vCombination(c,c).
vCombination(c,w).
vCombination(s,w).
vCombination(w,u).
vCombination(w,l).
vCombination(w,r).
vCombination(w,c).
vCombination(w,s).



% function waterBorders(+Grid, -NewGrid) receives grid and surrounds it with water
waterBorders(Grid, NewGrid):-
	verticalBorders(Grid,Temp),horizontalBorders(Temp,NewGrid).

% function verticalBoders(+Grid, -NewGrid) receives grid and adds columns of water on sides
verticalBorders([],[]).
verticalBorders([Row|Grid],[NewRow|NewGrid]):-
	verticalBorders(Grid,NewGrid),
	append([w],Row,Temp),
	append(Temp,[w],NewRow).

% function horizontalBorders(+Grid, -NewGrid) adds rows of of water at the top and bottom of a grid
horizontalBorders([Line|Grid],NewGrid):-
	createWaterLine(Line,WaterLine),
	append([WaterLine],[Line|Grid],Temp),
	append(Temp,[WaterLine],NewGrid).

% function createWaterLine(+Line, -NewLine) creates line the same length as input line, but only contains water
createWaterLine([_],[w]).
createWaterLine([_|Line],[w|WaterLine]):-createWaterLine(Line,WaterLine).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% checkGrid(?Grid) adds water to all sides of grid and checks it
checkGrid(Grid):-waterBorders(Grid,NewGrid),checkAllCells(NewGrid,NewGrid).

% checkAllCells(?Grid, ?Grid) check if all the cells in the grid have correct adjacent cells
checkAllCells(_, [[w, C12, w], [C21, C22, w], [w, w, w]]) :-
    checkCell([[w, C12, w], [C21, C22, w], [w, w, w]]).

checkAllCells(_, [[_, _, w], [_, w, w], [w, w, w]]).

checkAllCells(Grid, [[w, C12, w | Tail1], [C21, C22, C23 | Tail2], [w, w, w | Tail3]]) :-
    checkCell([[w, C12, w], [C21, C22, C23], [w, w, w]]),
    checkAllCells(Grid, [[C12, w | Tail1], [C22, C23 | Tail2], [w, w | Tail3]]).
   
checkAllCells(Grid, [[_, C12, C13 | Tail1], [_, w, C23 | Tail2], [w, w, w | Tail3]]) :-
    checkAllCells(Grid, [[C12, C13 | Tail1], [w, C23 | Tail2], [w, w | Tail3]]).

checkAllCells([_, SecondGridRow | OtherGridRows], [[w, C12, w], [C21, C22, w], [w, C32, w] | _]) :-
    checkCell([[w, C12, w], [C21, C22, w], [w, C32, w]]),
    checkAllCells([SecondGridRow | OtherGridRows], [SecondGridRow | OtherGridRows]).

checkAllCells([_, SecondGridRow | OtherGridRows], [[_, _, w], [_, w, w], [_, _, w] | _]) :-
    checkAllCells([SecondGridRow | OtherGridRows], [SecondGridRow | OtherGridRows]).
 
checkAllCells(Grid, [[w, C12, w | Tail1], [C21, C22, C23 | Tail2], [w, C32, w | Tail3] | Tail]) :-
    checkCell([[w, C12, w], [C21, C22, C23], [w, C32, w]]),
    checkAllCells(Grid, [[C12, w | Tail1], [C22, C23 | Tail2], [C32, w | Tail3] | Tail]).

checkAllCells(Grid, [[_, C12, C13 | Tail1], [_, w, C23 | Tail2], [_, C32, C33 | Tail3] | Tail]) :-
    checkAllCells(Grid, [[C12, C13 | Tail1], [w, C23 | Tail2], [C32, C33 | Tail3] | Tail]).


% checkCell(+Cell) receives 9 cells and check if a cell in the center has the correct adjacent cells
checkCell([[w, w, w], [C21, c, C23], [w, w, w]]) :-
    ship(C21), ship(C23),
    hCombination(C21, c),
    hCombination(c, C23).
    
checkCell([[w, w, w], [C21, C22, C23], [w, w, w]]) :-
    \+(C22 == c), ship(C22), (ship(C21); ship(C23)),
    hCombination(C21, C22),
    hCombination(C22, C23).

checkCell([[w, C12, w], [w, c, w], [w, C32, w]]) :-
    ship(C12), ship(C32),
    vCombination(C12, c),
    vCombination(c, C32).

checkCell([[w, C12, w], [w, C22, w], [w, C32, w]]) :-
    \+(C22 == c), ship(C22), (ship(C12); ship(C32)),
    vCombination(C12, C22),
    vCombination(C22, C32).
    
checkCell([[w, w, w], [w, s, w], [w, w, w]]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% checkHorizontal(+Grid,+HorizontalClues) checks if the grid fits horizontal clues
checkHorizontal([Line],[Amount]):-checkLine(Line,Amount).
checkHorizontal([Line|Grid],[Amount|Rest]):-checkLine(Line,Amount),checkHorizontal(Grid,Rest),!.

% checkvertical(+Grid,+VerticalClues) checks if the grid fits vertical clues
checkVertical(_,[]).
checkVertical(Grid,[Amount|Rest]):-
	getColumn(Grid,Column,NewGrid),
	checkLine(Column,Amount),
	checkVertical(NewGrid,Rest),!.

% chechkLine(+Line, +Amount) checks if there is correct amount of ships in line
checkLine([],0).
checkLine([Cell|Line],Amount):-
	\+(Cell==w),
	Amount > 0,
	NewAmount is Amount - 1,
	checkLine(Line,NewAmount).
checkLine([w|Line],Amount):-
	checkLine(Line,Amount).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%


% checkShips(+Grid,+Ships) check whether grid contains correct ships
checkShips([[]|_],[]).
checkShips(Grid,Ships):-removeColumn(Grid,ColumnGrid),checkShips(Grid,ColumnGrid,Ships).

checkShips([],ColumnGrid,Ships):-checkShips(ColumnGrid,Ships).
checkShips([[u|_]|Grid],ColumnGrid,Ships):-
	getColumn(Grid,Column,_),
	checkShip(1,Column,Ships,NewShips),
	checkShips(Grid,ColumnGrid,NewShips),!.
checkShips([[l|Line]|Grid],ColumnGrid,Ships):-
	checkShip(1,Line,Ships,NewShips),
	checkShips(Grid,ColumnGrid,NewShips),!.
checkShips([[s|_]|Grid],ColumnGrid,Ships):-
	deleteOne(1,Ships,NewShips),
	checkShips(Grid,ColumnGrid,NewShips),!.
checkShips([[_|_]|Grid],ColumnGrid,Ships):-
	checkShips(Grid,ColumnGrid,Ships).


% checkShip(+Length, +Line, +Ships, -NewShips) checks whether a list Ships contains a ship
% at the begginning of the line and removes the ship it found on the line
checkShip(Lengt,[],Ships,NewShips):-deleteOne(Lengt,Ships,NewShips).
checkShip(Lengt,[Cell|Line],Ships,NewShips):-
	ship(Cell),
	NewLength is Lengt + 1,
	checkShip(NewLength,Line,Ships,NewShips).
checkShip(Lengt,[Cell|_],Ships,NewShips):-
	\+ship(Cell),
	deleteOne(Lengt,Ships,NewShips).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%getColumn(+Matrix, -Column, -RemainderMatrix) recieves matrix and splits it into its first column and the rest of the matrix
getColumn([], [], []).
getColumn([[First|List]|Rest], [First|Firsts], [List|NewRest]) :-
	getColumn(Rest, Firsts, NewRest).

%getColumn(+Matrix, -NewMatrix) removes first column from matrix
removeColumn([],[]).
removeColumn([[_|List]|Rest],[List|NewRest]):-
	removeColumn(Rest,NewRest).

water(w).

% printGrid(+Grid) prints out the grid
printGrid([]).
printGrid([[]|Grid]):-nl,printGrid(Grid).
printGrid([[w|Line]|Grid]):-print(-),printGrid([Line|Grid]).
printGrid([[_|Line]|Grid]):- print(#),printGrid([Line|Grid]).



% delete_one(+Term, +List, -NewList) removes first occurence of term in list
deleteOne(_, [], []):-fail.
deleteOne(Term, [Term|Tail], Tail):-!.
deleteOne(Term, [Head|Tail], [Head|Result]) :-
  deleteOne(Term, Tail, Result).

