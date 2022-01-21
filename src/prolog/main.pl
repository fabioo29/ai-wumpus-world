%   ----------------- %    H -> hunter
% 4 |   |   |   | P | %    P -> pit
%   ----------------- %    W -> wumpus
% 3 | W | G | P |   | %    G -> gold
%   ----------------- %     
% 2 |   |   |   |   | %    GOAL: Hunter has to take gold and get out of the cavern through [1,1] position
%   ----------------- %    without being killed by the wumpus or fall into a pit. Hunter can kill the wumpus 
% 1 | H |   | P |   | %    with an arrow. Hunter actions are: 'move_forward', 'turn_left', 'turn_right', 'shoot'.
% y ----------------- %    wumpus screams when it is killed. Hunter can sense wumpus, gold or pit in adjacent cells.
%   x 1   2   3   4   %    AGENT scores when he: gets gold(+1000), dies(-1000), takes_action(-1), shoot_arrow(-10).

% DEBUG MODE
:- debug.

% ENCODING LANGUAGE
:- encoding(utf8).

% DELETE DYNAMIC DATA 
% (w_ = world data, a_ = agent data, h_ = hunter data)
:- abolish(w_wall/2). % wall position
:- abolish(w_hunter/3). % hunter position
:- abolish(w_wumpus/2). % wumpus position
:- abolish(w_pit/2). % pit position
:- abolish(w_gold/2). % gold position
:- abolish(w_goal/1). % world goal done?
:- abolish(w_cells/1). % available cells
:- abolish(h_arrow/1). % arrow available?
:- abolish(h_score/1). % hunter score
:- abolish(a_range/3). % save range actions
:- abolish(a_visited/2). % save visited cells
:- abolish(no_logs/1). % no logs

% CREATE DYNAMIC DATA
:- dynamic ([
    w_wall/2, 
    w_hunter/3, 
    w_wumpus/2, 
    w_pit/2, 
    w_gold/2, 
    w_goal/1, 
    w_cells/1,
    h_score/1, 
    h_arrow/1,
    a_range/3,
    a_visited/2,
    no_logs/1
]).

% CLEAR WORLD
clearWorld :-
    retractall(w_wall(_,_)),
    retractall(w_hunter(_,_,_)),
    retractall(w_wumpus(_,_)),
    retractall(w_pit(_,_)),
    retractall(w_gold(_,_)),
    retractall(h_arrow(_)),
    retractall(w_goal(_)),
    retractall(h_score(_)),
    retractall(w_cells(_)),
    retractall(a_range(_,_,_)),
    retractall(a_visited(_,_)),
    retractall(no_logs(_)). 

% BUILD BORDER MAP WALLS
buildWalls :-
    assert(w_wall(0,0)),
    assert(w_wall(0,1)),
    assert(w_wall(0,2)),
    assert(w_wall(0,3)),
    assert(w_wall(0,4)),
    assert(w_wall(0,5)),
    assert(w_wall(1,5)),
    assert(w_wall(2,5)),
    assert(w_wall(3,5)),
    assert(w_wall(4,5)),
    assert(w_wall(5,5)),
    assert(w_wall(5,4)),
    assert(w_wall(5,3)),
    assert(w_wall(5,2)),
    assert(w_wall(5,1)),
    assert(w_wall(5,0)),
    assert(w_wall(4,0)),
    assert(w_wall(3,0)),
    assert(w_wall(2,0)),
    assert(w_wall(1,0)).

% SELECT RANDOM AVAILABLE CELL
selectCell(X,Y) :-
    w_cells(Cells),
	random_select(Cell,Cells,NewCells),
    Cell = [X,Y],
	retract(w_cells(_)),
	assert(w_cells(NewCells)).

% BUILD WUMPUS POSITION
buildWumpus :- 
    selectCell(X,Y),
    assert(w_wumpus(X,Y)).

% BUILD PITS POSITION
buildPits :-
    selectCell(X1,Y1),
    assert(w_pit(X1,Y1)),
    selectCell(X2,Y2),
    assert(w_pit(X2,Y2)),
    selectCell(X3,Y3),
    assert(w_pit(X3,Y3)).

% BUILD GOLD POSITION
buildGold :-
    selectCell(X,Y),
    assert(w_gold(X,Y)).

% CREATE WORLD
createWorld :-    
    assert(w_cells([[1,3],[1,4],[2,2],[2,3],[2,4],[3,1],[3,2],[3,3],[3,4],[4,1],[4,2],[4,3],[4,4]])),
    buildWalls,
    buildWumpus,
    buildPits,
    buildGold,
    assert(w_hunter(1,1,right)),
    assert(h_arrow(1)),
    assert(w_goal(0)),
    assert(h_score(0)).

% PRINT WELCOME MESSAGE
welcome :-
    format('\n\n~`=t~60|\n'),
    format(' |~t~a~t~58+| ', ['[PROLOG] Wumpus-world AI Agent']),
    format('\n~`=t~60|'),
    format('\n |~t~a~t~58+| ', ['GOAL: get the gold and return to this position']),
    format('\n |~t~a~t~58+| ', ['ACTIONS: ready, move, left, right, grab, shoot, climb']),
    format('\n |~t~a~t~58+| ', ['AVOID: wumpus, the walls and all the pits in the map']),
    format('\n |~t~a~t~58+| \n\n', ['BONUS: aim to the wumpus and kill it with the arrow']).

% INITIALIZE GAME
init :- write('Type "ready." to start the game: '), read(X), X = ready, !.
init :- init.

% HUNTER HIT SOMETHING?
hunterHit([0,0,0]).
hunterHit([1, _, _]):- 
    h_score(SCORE), (no_logs(NL), NL \= 1 -> write('\n\nGAME OVER: Wumpus killed you!\n'); true), 
    h_score(A), B is A-1000, retract(h_score(_)), assert(h_score(B)),
    (no_logs(NL), NL \= 1 -> format('YOUR SCORE: ~d point(s).\n',[SCORE]); true), !, fail.
hunterHit([_, 1, _]):-
    h_score(SCORE), (no_logs(NL), NL \= 1 -> write('\n\nGAME OVER: You fell into a pit!\n'); true), 
    h_score(A), B is A-1000, retract(h_score(_)), assert(h_score(B)),
    (no_logs(NL), NL \= 1 -> format('YOUR SCORE: ~d point(s).\n',[SCORE]); true), !, fail.
hunterHit([_, _, 1]):-
    (no_logs(NL), NL \= 1 -> write('\n\nWARNING: You hit the wall!'); true), w_hunter(X,Y,FACING),
    (
        FACING = up, N_Y is Y-1, N_X is X;
        FACING = down, N_Y is Y+1, N_X is X;
        FACING = left, N_X is X+1, N_Y is Y;
        FACING = right, N_X is X-1, N_Y is Y
    ),
    retract(w_hunter(_,_,_)),
    assert(w_hunter(N_X,N_Y,FACING)).

% CHECK HUNTER INTERCEPTIONS - WUMPUS
getPerceptions(P_goal) :-
    w_hunter(X,Y,_),
    (w_wumpus(X,Y) -> P_wumpus is 1; P_wumpus is 0),
    (w_pit(X,Y) -> P_pit is 1; P_pit is 0),
    (w_wall(X,Y) -> P_wall is 1; P_wall is 0),
    (w_hunter(1,1,_), w_goal(1) -> P_goal is 1; P_goal is 0),
    hunterHit([P_wumpus,P_pit,P_wall]).

% GET HUNTER CURRENT POSITION
printHunterPosition :- w_hunter(X,Y,FACING), format('\nHunter position: (~d,~d), facing ~s.\n', [X, Y, FACING]).

% PRINT HUNTER SCORE
printScore :- h_score(SCORE), format('Current score: ~d point(s).\n', [SCORE]).

% STENCH SENSOR INFO - WUMPUS NEAR
stench(X,Y) :-
    w_wumpus(A,B),
    (
        X is A,(Y is B-1;Y is B+1);
	    Y is B,(X is A-1;X is A+1)
    ).

% BREEZE SENSOR INFO - PIT NEAR
breeze(X,Y) :-
    w_pit(A,B),
    (
        X is A,(Y is B-1;Y is B+1);
        Y is B,(X is A-1;X is A+1)
    ).

% GLITTER SENSOR INFO - GOLD NEAR
glitter(X,Y) :- w_gold(X,Y).

% GET HUNTER SENSORS INFO
getSensors(SENSORS) :-
    w_hunter(X,Y,_),
    (stench(X,Y) -> S_stench is 1; S_stench is 0),
    (breeze(X,Y) -> S_breeze is 1; S_breeze is 0),
    (glitter(X,Y) -> S_glitter is 1; S_glitter is 0),
    SENSORS = [S_stench, S_breeze, S_glitter].

% PRINT SENSORS INFO
printInfo(SENSORS) :- format('stench: ~d breeze: ~d glitter: ~d\n', SENSORS).

% HUNTER CONTROL - MOVE FORWARD
move :- 
    h_score(A), B is A-1, retract(h_score(_)), assert(h_score(B)),
    w_hunter(X,Y,FACING),
    (
        FACING = up, plus(Y,+1,N_Y), N_X is X;
        FACING = down, plus(Y,-1,N_Y), N_X is X;
        FACING = left, plus(X,-1,N_X), N_Y is Y;
        FACING = right, plus(X,+1,N_X), N_Y is Y
    ),
	retract(w_hunter(_,_,_)),
	assert(w_hunter(N_X, N_Y, FACING)). 

% HUNTER CONTROL - ROTATE LEFT
left :-
    h_score(A), B is A-1, retract(h_score(_)), assert(h_score(B)),
    w_hunter(X,Y,FACING),
    (
        FACING = up, NEW_FACING = left;
        FACING = down, NEW_FACING = right;
        FACING = left, NEW_FACING = down;
        FACING = right, NEW_FACING = up
    ),
    retract(w_hunter(_,_,_)),
    assertz(w_hunter(X,Y,NEW_FACING)).

% HUNTER CONTROL - ROTATE RIGHT
right :-
    h_score(A), B is A-1, retract(h_score(_)), assert(h_score(B)),
    w_hunter(X,Y,FACING),
    (
        FACING = up, NEW_FACING = right;
        FACING = down, NEW_FACING = left;
        FACING = left, NEW_FACING = up;
        FACING = right, NEW_FACING = down
    ),
    retract(w_hunter(_,_,_)),
    assertz(w_hunter(X,Y,NEW_FACING)).

% HUNTER CONTRL - GRAB GOLD
grab(_) :- h_score(A), B is A-1, retract(h_score(_)), assert(h_score(B)), fail.
grab(1) :- 
    (no_logs(NL), NL \= 1 -> write('\n\nGOAL: You already grabbed all the gold.'); true), !.
grab(0) :-
    w_hunter(X,Y,_),
    w_gold(X,Y),
    retract(w_gold(X,Y)),
    retract(w_goal(0)),
    assert(w_goal(1)),
    h_score(A), B is A+1000, retract(h_score(_)), assert(h_score(B)),
    (no_logs(NL), NL \= 1 -> write('\n\nGOAL: You grabbed the gold!. Now get out of the cave.'); true), !.
grab(0) :-
    (no_logs(NL), NL \= 1 -> write('\n\nWARNING: There`s no gold to grab where you are.'); true).

% HUNTER CONTROL - SHOOT ARROW
shoot :- h_score(A), B is A-10, retract(h_score(_)), assert(h_score(B)), fail.
shoot :- h_arrow(0), (no_logs(NL), NL \= 1 -> write('\n\nWARNING: You have no arrows to shoot.'); true), !.
shoot :- 
    w_hunter(X,Y,FACING),
    w_wumpus(A,B),
    retract(h_arrow(1)),
    assert(h_arrow(0)),
    (
        FACING = up, X = A, Y < B;
        FACING = down, X = A, Y > B;
        FACING = left, Y = B, X > A;
        FACING = right, Y = B, X < A
    ),
    retract(w_wumpus(_,_)),
    (no_logs(NL), NL \= 1 -> write('\n\nBONUS: Wumpus scream which means you killed him!'); true), !.
shoot :-
    (no_logs(NL), NL \= 1 -> write('\n\nWARNING: Your arrow missed the wumpus!'); true).

% HUNTER CONTROL - CLIMB ARROW
climb(_):- h_score(A), B is A-1, retract(h_score(_)), assert(h_score(B)), fail.
climb(0) :- 
    (no_logs(NL), NL \= 1 -> write('\n\nWARNING: You are in no conditions to get out of the cave.'); true), !.
climb(1):-
    h_score(SCORE),
    (no_logs(NL), NL \= 1 -> write('\n\nWINNER: You managed to get out of the cave with the gold!\n'); true),
    (no_logs(NL), NL \= 1 -> format('YOUR SCORE: ~d point(s).\n',[SCORE]); true).

% TAKE HUNTER ACTION
action(move,_) :- move, !.
action(left,_) :- left, !.
action(right,_) :- right, !.
action(grab,GOAL) :- grab(GOAL), !.
action(shoot,_) :- shoot, !.
action(climb,GOAL) :- (GOAL = 1 -> climb(GOAL), !, fail; climb(GOAL), !).
action(_,_) :- write('UNKNOWN ACTION: Please use move, left, right, grab or shoot.\n').

% SHOW MENU TO USER
menu :-
    getPerceptions(GOAL),
    printHunterPosition,
    getSensors(SENSORES),
    printInfo(SENSORES),
    printScore,
    write('Next action: '), 
    read(OPTION),
    action(OPTION, GOAL),
    menu.

% AGENT NEXT ACTION
getStep(HX, HY, up, [NX, NY], ACTION) :- NX is HX, NY is HY + 1, ACTION = move, !.
getStep(HX, HY, up, [NX, NY], ACTION) :- NX is HX, NY is HY - 1, ACTION = right, retract(a_range(HX,HY,COST)), NC is COST-25, assert(a_range(HX,HY,NC)), !.
getStep(HX, HY, up, [NX, NY], ACTION) :- NX is HX - 1, NY is HY, ACTION = left, retract(a_range(HX,HY,COST)), NC is COST-25, assert(a_range(HX,HY,NC)), !.
getStep(HX, HY, up, [NX, NY], ACTION) :- NX is HX + 1, NY is HY, ACTION = right, retract(a_range(HX,HY,COST)), NC is COST-25, assert(a_range(HX,HY,NC)), !.

getStep(HX, HY, right, [NX, NY], ACTION) :- NX is HX + 1, NY is HY, ACTION = move, !.
getStep(HX, HY, right, [NX, NY], ACTION) :- NX is HX, NY is HY - 1, ACTION = right, retract(a_range(HX,HY,COST)), NC is COST-25, assert(a_range(HX,HY,NC)), !.
getStep(HX, HY, right, [NX, NY], ACTION) :- NX is HX, NY is HY + 1, ACTION = left, retract(a_range(HX,HY,COST)), NC is COST-25, assert(a_range(HX,HY,NC)), !.
getStep(HX, HY, right, [NX, NY], ACTION) :- NX is HX - 1, NY is HY, ACTION = right, retract(a_range(HX,HY,COST)), NC is COST-25, assert(a_range(HX,HY,NC)), !.

getStep(HX, HY, down, [NX, NY], ACTION) :- NX is HX, NY is HY -1, ACTION = move, !.
getStep(HX, HY, down, [NX, NY], ACTION) :- NX is HX, NY is HY + 1, ACTION = right, retract(a_range(HX,HY,COST)), NC is COST-25, assert(a_range(HX,HY,NC)), !.
getStep(HX, HY, down, [NX, NY], ACTION) :- NX is HX + 1, NY is HY, ACTION = left, retract(a_range(HX,HY,COST)), NC is COST-25, assert(a_range(HX,HY,NC)), !.
getStep(HX, HY, down, [NX, NY], ACTION) :- NX is HX - 1, NY is HY, ACTION = right, retract(a_range(HX,HY,COST)), NC is COST-25, assert(a_range(HX,HY,NC)), !.

getStep(HX, HY, left, [NX, NY], ACTION) :- NX is HX - 1, NY is HY, ACTION = move, !.
getStep(HX, HY, left, [NX, NY], ACTION) :- NX is HX, NY is HY + 1, ACTION = right, retract(a_range(HX,HY,COST)), NC is COST-25, assert(a_range(HX,HY,NC)), !.
getStep(HX, HY, left, [NX, NY], ACTION) :- NX is HX, NY is HY - 1, ACTION = left, retract(a_range(HX,HY,COST)), NC is COST-25, assert(a_range(HX,HY,NC)), !.
getStep(HX, HY, left, [NX, NY], ACTION) :- NX is HX + 1, NY is HY, ACTION = right, retract(a_range(HX,HY,COST)), NC is COST-25, assert(a_range(HX,HY,NC)), !.

% AGENT BEST CELL OPTION
cellsCost(X,Y, up, 0, COST) :- U is Y+1, \+w_wall(X,U), a_range(X,U,COST).
cellsCost(X,Y, down, 0, COST) :- D is Y-1, \+w_wall(X,D), a_range(X,D,COST).
cellsCost(X,Y, left, 0, COST) :- L is X-1, \+w_wall(L,Y), a_range(L,Y,COST).
cellsCost(X,Y, right, 0, COST) :- R is X+1, \+w_wall(R,Y), a_range(R,Y,COST).
cellsCost(X,Y, _, 0, COST) :- L is X-1, \+w_wall(L,Y), a_range(L,Y,COST).
cellsCost(X,Y, _, 0, COST) :- U is Y+1, \+w_wall(X,U), a_range(X,U,COST).
cellsCost(X,Y, _, 0, COST) :- R is X+1, \+w_wall(R,Y), a_range(R,Y,COST).
cellsCost(X,Y, _, 0, COST) :- D is Y-1, \+w_wall(X,D), a_range(X,D,COST).

cellsCost(X,Y, up, 1, COST) :- U is Y+1, \+w_wall(X,U), a_visited(X,U), a_range(X,U,COST).
cellsCost(X,Y, down, 1, COST) :- D is Y-1, \+w_wall(X,D), a_visited(X,D), a_range(X,D,COST).
cellsCost(X,Y, left, 1, COST) :- L is X-1, \+w_wall(L,Y), a_visited(L,Y), a_range(L,Y,COST).
cellsCost(X,Y, right, 1, COST) :- R is X+1, \+w_wall(R,Y), a_visited(R,Y), a_range(R,Y,COST).
cellsCost(X,Y, _, 1, COST) :- L is X-1, \+w_wall(L,Y), a_visited(L,Y), a_range(L,Y,COST).
cellsCost(X,Y, _, 1, COST) :- U is Y+1, \+w_wall(X,U), a_visited(X,U), a_range(X,U,COST).
cellsCost(X,Y, _, 1, COST) :- R is X+1, \+w_wall(R,Y), a_visited(R,Y), a_range(R,Y,COST).
cellsCost(X,Y, _, 1, COST) :- D is Y-1, \+w_wall(X,D), a_visited(X,D), a_range(X,D,COST).

% HELPER FIND ADJACENT CELL WITH SPECIFIC COST
findCell(X,Y,up,COST,CELL) :- a_range(NX, NY, COST), NX = X, TY is Y + 1, NY = TY, CELL = [NX, NY], !.
findCell(X,Y,right,COST,CELL) :- a_range(NX, NY, COST), TX is X + 1, NX = TX, NY = Y, CELL = [NX, NY], !.
findCell(X,Y,down,COST,CELL) :- a_range(NX, NY, COST), NX = X, TY is Y - 1, NY = TY, CELL = [NX, NY], !.
findCell(X,Y,left,COST,CELL) :- a_range(NX, NY, COST), TX is X - 1, NX = TX, NY = Y, CELL = [NX, NY], !.
findCell(X,Y,_,COST,CELL) :- a_range(NX, NY, COST), TX is X - 1, NX = TX, NY = Y, CELL = [NX, NY], !.
findCell(X,Y,_,COST,CELL) :- a_range(NX, NY, COST), NX = X, TY is Y + 1, NY = TY, CELL = [NX, NY], !.
findCell(X,Y,_,COST,CELL) :- a_range(NX, NY, COST), TX is X + 1, NX = TX, NY = Y, CELL = [NX, NY], !.
findCell(X,Y,_,COST,CELL) :- a_range(NX, NY, COST), NX = X, TY is Y - 1, NY = TY, CELL = [NX, NY], !.

% AGENT TAKE NEXT STEP
nextMove(ACTION, GOAL) :- 
    w_hunter(X,Y,FACING),
    findall(C, cellsCost(X, Y, FACING, GOAL, C), COSTS),
    (GOAL = 0 -> min_list(COSTS, COST); max_list(COSTS, COST)),
    findCell(X, Y, FACING, COST, CELL),
    getStep(X,Y, FACING, CELL, ACTION).

% UPDATE AGENT DATABASE
knowledge(X, Y, stench) :- L is X - 1, \+w_wall(L,Y), \+a_visited(L,Y), ((a_range(L,Y,C), C \= 25) -> (C = 975 -> N_C is 1000-975; N_C is 1000), retract(a_range(L,Y,_)), assert(a_range(L,Y,N_C)); assert(a_range(L,Y,1000))), fail.
knowledge(X, Y, stench) :- U is Y + 1, \+w_wall(X,U), \+a_visited(X,U), ((a_range(X,U,C), C \= 25) -> (C = 975 -> N_C is 1000-975; N_C is 1000), retract(a_range(X,U,_)), assert(a_range(X,U,N_C)); assert(a_range(X,U,1000))), fail.
knowledge(X, Y, stench) :- R is X + 1, \+w_wall(R,Y), \+a_visited(R,Y), ((a_range(R,Y,C), C \= 25) -> (C = 975 -> N_C is 1000-975; N_C is 1000), retract(a_range(R,Y,_)), assert(a_range(R,Y,N_C)); assert(a_range(R,Y,1000))), fail.
knowledge(X, Y, stench) :- D is Y - 1, \+w_wall(X,D), \+a_visited(X,D), ((a_range(X,D,C), C \= 25) -> (C = 975 -> N_C is 1000-975; N_C is 1000), retract(a_range(X,D,_)), assert(a_range(X,D,N_C)); assert(a_range(X,D,1000))), fail; true.

knowledge(X, Y, breeze) :- L is X - 1, \+w_wall(L,Y), \+a_visited(L,Y), ((a_range(L,Y,C), C \= 25) -> (C = 1000 -> N_C is 1000-975; N_C is 975), retract(a_range(L,Y,_)), assert(a_range(L,Y,N_C)); assert(a_range(L,Y,975))), fail.
knowledge(X, Y, breeze) :- U is Y + 1, \+w_wall(X,U), \+a_visited(X,U), ((a_range(X,U,C), C \= 25) -> (C = 1000 -> N_C is 1000-975; N_C is 975), retract(a_range(X,U,_)), assert(a_range(X,U,N_C)); assert(a_range(X,U,975))), fail.
knowledge(X, Y, breeze) :- R is X + 1, \+w_wall(R,Y), \+a_visited(R,Y), ((a_range(R,Y,C), C \= 25) -> (C = 1000 -> N_C is 1000-975; N_C is 975), retract(a_range(R,Y,_)), assert(a_range(R,Y,N_C)); assert(a_range(R,Y,975))), fail.
knowledge(X, Y, breeze) :- D is Y - 1, \+w_wall(X,D), \+a_visited(X,D), ((a_range(X,D,C), C \= 25) -> (C = 1000 -> N_C is 1000-975; N_C is 975), retract(a_range(X,D,_)), assert(a_range(X,D,N_C)); assert(a_range(X,D,975))), fail; true.

knowledge(X, Y, safe) :- L is X - 1, \+w_wall(L,Y), \+a_visited(L,Y), (a_range(L,Y,_) -> retract(a_range(L,Y,_)); true), assert(a_range(L,Y,0)), fail.
knowledge(X, Y, safe) :- U is Y + 1, \+w_wall(X,U), \+a_visited(X,U), (a_range(X,U,_) -> retract(a_range(X,U,_)); true), assert(a_range(X,U,0)), fail.
knowledge(X, Y, safe) :- R is X + 1, \+w_wall(R,Y), \+a_visited(R,Y), (a_range(R,Y,_) -> retract(a_range(R,Y,_)); true), assert(a_range(R,Y,0)), fail.
knowledge(X, Y, safe) :- D is Y - 1, \+w_wall(X,D), \+a_visited(X,D), (a_range(X,D,_) -> retract(a_range(X,D,_)); true), assert(a_range(X,D,0)), fail; true.

% AGENT HEURISTIC 
heuristic(_,_) :- w_hunter(X,Y,_), w_goal(0), (a_range(X,Y,COST) -> retract(a_range(X,Y,COST)), N_COST is COST + 25, assert(a_range(X,Y,N_COST)); assert(a_range(X,Y,25))), fail.
heuristic(_,_) :- w_hunter(X,Y,_), w_goal(1), (a_range(X,Y,COST) -> retract(a_range(X,Y,COST)), N_COST is COST - 25, assert(a_range(X,Y,N_COST)); assert(a_range(X,Y,25))), fail.
heuristic(_,_) :- w_hunter(X,Y,_), \+a_visited(X,Y), assert(a_visited(X,Y)), fail.
heuristic([_,_,_], climb) :- w_hunter(1,1,_), \+w_gold(_,_), !.
heuristic([_,_,1], grab) :- !.
heuristic([_,_,_], shoot) :- fail, !.
heuristic([1,_,_], OPTION) :- w_hunter(X,Y,_), knowledge(X, Y, stench), w_goal(GOAL), nextMove(OPTION, GOAL), !.
heuristic([_,1,_], OPTION) :- w_hunter(X,Y,_), knowledge(X, Y, breeze), w_goal(GOAL), nextMove(OPTION, GOAL), !.
heuristic([_,_,_], OPTION) :- w_hunter(X,Y,_), knowledge(X, Y, safe), w_goal(GOAL), nextMove(OPTION, GOAL), !.

% PRINT ADJACENT CELLS COST 
printRange :- write('Costs: '), fail.
printRange :- w_hunter(X,Y,_), L is X-1, (a_range(L,Y,LEFT) -> format('[l: ~w] ', LEFT); format('[l: NA] ')), fail.
printRange :- w_hunter(X,Y,_), U is Y+1, (a_range(X,U,UP) -> format('[u: ~w] ', UP); format('[u: NA] ')), fail.
printRange :- w_hunter(X,Y,_), R is X+1, (a_range(R,Y,RIGHT) -> format('[r: ~w] ', RIGHT); format('[r: NA] ')), fail.
printRange :- w_hunter(X,Y,_), D is Y-1, (a_range(X,D,DOWN) -> format('[d: ~w].~n', DOWN); format('[d: NA].~n')).

% RUN LOOP FOR THE AGENT TO RUN
runloop(STEP) :-
    getPerceptions(GOAL),
    getSensors(SENSORES),
    heuristic(SENSORES, OPTION),
    (no_logs(NL), NL \= 1 -> printHunterPosition; true),
    (no_logs(NL), NL \= 1 -> printRange; true),
    (no_logs(NL), NL \= 1 -> printInfo(SENSORES); true),
    (no_logs(NL), NL \= 1 -> printScore; true),
    (no_logs(NL), NL \= 1 -> write('Next action: '); true),
    (no_logs(NL), NL \= 1 -> format('~p.~n', [OPTION]); true),
    action(OPTION, GOAL),
    STEP \= -1, N_STEP is STEP + 1,
    runloop(N_STEP).

% RUN WUMPUS WORLD SIMULATION
run(pygame) :- clearWorld, assert(no_logs(1)),createWorld, !.
run(agent) :- clearWorld, assert(no_logs(0)),createWorld, runloop(0), !.
run :- clearWorld, assert(no_logs(0)), createWorld, welcome, init, menu. 