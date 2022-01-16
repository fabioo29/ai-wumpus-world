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

% DELETE DYNAMIC DATA
:- abolish(w_wall/2). % wall position
:- abolish(w_hunter/3). % hunter position
:- abolish(w_wumpus/2). % wumpus position
:- abolish(w_pit/2). % pit position
:- abolish(w_gold/2). % gold position
:- abolish(w_arrow/1). % arrow available?
:- abolish(w_goal/1). % gold available?

% CREATE DYNAMIC DATA
:- dynamic w_wall/2, w_hunter/3, w_wumpus/2, w_pit/2, w_gold/2, w_arrow/1, w_goal/1. 

% CLEAR WORLD
clearWorld :-
    retractall(w_wall(_,_)),
    retractall(w_hunter(_,_,_)),
    retractall(w_wumpus(_,_)),
    retractall(w_pit(_,_)),
    retractall(w_gold(_,_)),
    retractall(w_arrow(_)),
    retractall(w_goal(_)).

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

% CREATE WORLD
createWorld :-
    buildWalls,
    assert(w_hunter(1,1,right)),
    assert(w_wumpus(1,3)),
    assert(w_pit(3,1)),
    assert(w_pit(3,3)),
    assert(w_pit(4,4)),
    assert(w_gold(2,3)),
    assert(w_arrow(1)),
    assert(w_goal(0)).

% PRINT WELCOME MESSAGE
welcome :-
    format('\n\n~`=t~60|\n'),
    format(' |~t~a~t~58+| ', ['[PROLOG] Wumpus-world AI Agent']),
    format('\n~`=t~60|'),
    format('\n |~t~a~t~58+| ', ['ACTIONS: ready, move, left, right, grab, shoot']),
    format('\n |~t~a~t~58+| ', ['GOAL: get the gold and return to this position']),
    format('\n |~t~a~t~58+| ', ['AVOID: wumpus, the walls and all the pits in the map']),
    format('\n |~t~a~t~58+| \n\n', ['BONUS: aim to the wumpus and kill it with the arrow']).

% INITIALIZE GAME
init :- 
    write('Type "ready." to start the game: '),
    read(X),
    X = ready, !, true;
    init.

% CHECK HUNTER INTERCEPTIONS - WUMPUS
getPerceptions :-
    w_hunter(X,Y,FACING),
    (w_hunter(1,1,_), w_goal(1) -> P_goal is 1; P_goal is 0),
    (w_wumpus(X,Y) -> P_wumpus is 1; P_wumpus is 0),
    (w_pit(X,Y) -> P_pit is 1; P_pit is 0),
    (w_wall(X,Y) -> P_wall is 1; P_wall is 0),
    (
        P_goal = 1, write('\n\nWINNER: You managed to get out of the cave with the gold!\n'), !, fail;
        P_wumpus = 1, write('\n\nGAME OVER: Wumpus killed you!\n'), !, fail;
        P_pit = 1, write('\n\nGAME OVER: You fell into a pit!\n'), !, fail;
        (
            P_wall = 1,
            write('\n\nWARNING: You hit the wall!'),
            (
                FACING = up,N_Y is Y-1, N_X is X;
                FACING = down,N_Y is Y+1, N_X is X;
                FACING = left,N_X is X+1, N_Y is Y;
                FACING = right,N_X is X-1, N_Y is Y
            ),
            retractall(w_hunter(_,_,_)),
            assert(w_hunter(N_X,N_Y,FACING)), !, true
        )
    ); true.

% GET HUNTER CURRENT POSITION
printHunterPosition :-
    w_hunter(X,Y,FACING),
    format('\nhunter position: (~d,~d), facing ~s.\n', [X, Y, FACING]).

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
getSensors(SENSORES) :-
    w_hunter(X,Y,_),
    (stench(X,Y),S_stench is 1,!; S_stench is 0),
    (breeze(X,Y),S_breeze is 1,!; S_breeze is 0),
    (glitter(X,Y),S_glitter is 1,!; S_glitter is 0),
    SENSORES = [S_stench, S_breeze, S_glitter].

% PRINT SENSORS INFO
printInfo(SENSORES) :-
    format('stench: ~d breeze: ~d glitter: ~d\n', SENSORES).

% HUNTER CONTROL - MOVE FORWARD
move :- 
    w_hunter(X,Y,FACING),
    (
        FACING = up, plus(Y,+1,N_Y), N_X is X;
        FACING = down, plus(Y,-1,N_Y), N_X is X;
        FACING = left, plus(X,-1,N_X), N_Y is Y;
        FACING = right, plus(X,+1,N_X), N_Y is Y
    ),
	retractall(w_hunter(_,_,_)),
	assert(w_hunter(N_X, N_Y, FACING)). 

% HUNTER CONTROL - ROTATE LEFT
left :-
    w_hunter(X,Y,FACING),
    (
        FACING = up, NEW_FACING = left;
        FACING = down, NEW_FACING = right;
        FACING = left, NEW_FACING = down;
        FACING = right, NEW_FACING = up
    ),
    retractall(w_hunter(_,_,_)),
    assertz(w_hunter(X,Y,NEW_FACING)).

% HUNTER CONTROL - ROTATE RIGHT
right :-
    w_hunter(X,Y,FACING),
    (
        FACING = up, NEW_FACING = right;
        FACING = down, NEW_FACING = left;
        FACING = left, NEW_FACING = up;
        FACING = right, NEW_FACING = down
    ),
    retractall(w_hunter(_,_,_)),
    assertz(w_hunter(X,Y,NEW_FACING)).

% HUNTER CONTRL - GRAB GOLD
grab :-
    w_goal(1), write('\n\nGOAL: You already grabbed all the gold.'), !;
    w_goal(0),
    w_hunter(X,Y,_),
    w_gold(X,Y),
    retract(w_gold(X,Y)),
    retract(w_goal(0)),
    assert(w_goal(1)),
    write('\n\nGOAL: You grabbed the gold!. Now get out of the cave.'), !;
    write('\n\nWARNING: There`s no gold to grab where you are.').

% HUNTER CONTROL - SHOOT ARROW
shoot :-
    w_arrow(0), write('\n\nWARNING: You have no arrows to shoot.'), !, true;
    (
        w_hunter(X,Y,FACING),
        w_wumpus(A,B),
        retract(w_arrow(1)),
        assert(w_arrow(0)),
        (
            FACING = up, X = A, Y < B;
            FACING = down, X = A, Y > B;
            FACING = left, Y = B, X > A;
            FACING = right, Y = B, X < A
        ), 
        retract(w_wumpus(_,_)),
        assert(w_wumpus(0,0)),
        write('\n\nBONUS: Wumpus scream which means you killed him!')
    ), !, true;

    write('\n\nWARNING: Your arrow missed the wumpus!'),
    retract(w_arrow(1)),
    assert(w_arrow(0)); true.

% TAKE HUNTER ACTION
action(OPTION) :-
    (
        OPTION = move, move, !;
        OPTION = left, left, !;
        OPTION = right, right, !;
        OPTION = grab, grab, !;
        OPTION = shoot, shoot, !;
        write('UNKNOWN ACTION: Please use move, left, right, grab or shoot.\n')
    ).

% SHOW MENU TO USER
menu :-
    getPerceptions,
    printHunterPosition,
    getSensors(SENSORES),
    printInfo(SENSORES),
    write('Next action: '), 
    read(OPTION),
    action(OPTION),
    menu.

% RUN WUMPUS WORLD SIMULATION
run :- 
    clearWorld,
    createWorld,
    welcome,
    init,
    menu. 