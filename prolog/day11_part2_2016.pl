:- dynamic state/5.

% Entry point to read the input and start the process
main :-
    read_input('input.txt', InitialState),
    assert(state(InitialState, 1, 0, [], [])),
    solve.

% Read input from the file
read_input(File, State) :-
    open(File, read, Stream),
    read_lines(Stream, Lines),
    close(Stream),
    parse_lines(Lines, State).

read_lines(Stream, []) :- at_end_of_stream(Stream).
read_lines(Stream, [Line|Lines]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, Line),
    read_lines(Stream, Lines).

parse_lines(Lines, State) :-
    % Parsing logic to convert lines into a state representation
    % This should create a structure representing floors and items
    % For simplicity, let's assume we have a predefined state
    State = [[hm, lm], [hg], [lg], []].  % Example state for testing

% Solve the problem
solve :-
    state(State, Floor, Steps, Inventory, History),
    (all_items_moved(State) ->
        format('Minimum steps: ~d~n', [Steps])
    ;
        next_states(State, Floor, Steps, Inventory, History, NextStates),
        findall(S, (member(NState, NextStates), assert(state(NState)), solve), _)
    ).

% Check if all items are moved to the fourth floor
all_items_moved(State) :-
    % Logic to check if all items are on the fourth floor
    % Example: check if State is in the form where all items are in the last list
    nth0(3, State, FourthFloor),
    length(FourthFloor, Length),
    Length > 0.

% Generate next possible states
next_states(State, Floor, Steps, Inventory, History, NextStates) :-
    % Logic to generate valid next states based on current state
    % This should involve moving items between floors and checking for safety
    % Here is a placeholder for generating next states
    findall(NewState, move(State, Floor, NewState), NextStates).

% Move items logic
move(State, Floor, NewState) :-
    % Logic to simulate moving items up or down
    % Ensure that the new state is valid according to the rules
    NewState = State.  % Placeholder

% Run the program
:- initialization(main).