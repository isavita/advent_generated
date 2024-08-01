:- use_module(library(clpfd)).
:- dynamic state/5.

% Read the initial state from the input file
read_input(File) :-
    open(File, read, Stream),
    read_lines(Stream, Floors),
    close(Stream),
    assert(state(Floors, 1, 0, 0, 0)). % Floors, Elevator position, Steps, RTGs, Microchips

read_lines(Stream, []) :-
    at_end_of_stream(Stream).
read_lines(Stream, [Line|Lines]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, LineString),
    split_string(LineString, " ", "", Line),
    read_lines(Stream, Lines).

% Main entry point to find the minimum steps
min_steps(Steps) :-
    read_input('input.txt'),
    state(Floors, Elevator, Steps, RTGs, Microchips),
    bfs([[Floors, Elevator, Steps, RTGs, Microchips]], Result),
    Result = [_, _, Steps | _].

% Breadth-first search for the solution
bfs([[State | Path] | _], Result) :-
    goal_state(State), !,
    reverse([State | Path], Result).
bfs([Current | Rest], Result) :-
    findall(Next, next_state(Current, Next), NextStates),
    append(Rest, NextStates, NewQueue),
    bfs(NewQueue, Result).

% Check if the goal state is reached
goal_state(Floors) :-
    Floors = [_, _, _, [_, _, _, _, _]].

% Generate the next valid state
next_state([Floors, Elevator, Steps, RTGs, Microchips], [NewFloors, NewElevator, NewSteps, NewRTGs, NewMicrochips]) :-
    move(Elevator, NewElevator),
    update_floors(Floors, Elevator, NewElevator, NewFloors, NewRTGs, NewMicrochips),
    NewSteps is Steps + 1,
    safe_state(NewFloors, NewRTGs, NewMicrochips).

% Move the elevator up or down
move(Elevator, NewElevator) :-
    ( Elevator < 4 -> NewElevator is Elevator + 1 ; NewElevator is Elevator - 1 ).

% Update the floors based on the elevator's movement
update_floors(Floors, Elevator, NewElevator, NewFloors, NewRTGs, NewMicrochips) :-
    nth1(Elevator, Floors, CurrentFloor),
    nth1(NewElevator, NewFloors, NewFloor),
    NewFloor = CurrentFloor, % Placeholder for moving items
    NewRTGs = RTGs, % Placeholder for RTGs
    NewMicrochips = Microchips. % Placeholder for Microchips

% Check if the new state is safe
safe_state(Floors, RTGs, Microchips) :-
    % Implement logic to ensure no unsafe configurations
    true. % Placeholder for safety check

% Run the program
:- initialization(main).
main :-
    min_steps(Steps),
    format('Minimum number of steps: ~w~n', [Steps]),
    halt.