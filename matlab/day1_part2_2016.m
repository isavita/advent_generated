
function main()
    % Reads instructions from input.txt and calculates the shortest path
    % to the destination and the first location visited twice.

    % Read instructions from the file
    instructions = read_instructions('input.txt');

    % --- Part One: Shortest path to the final destination ---
    [final_x, final_y] = calculate_final_position(instructions);
    shortest_distance = abs(final_x) + abs(final_y);
    fprintf('Part One: Shortest distance to the final destination is %d blocks.\n', shortest_distance);

    % --- Part Two: Distance to the first location visited twice ---
    distance_to_first_revisit = calculate_first_revisit_distance(instructions);
    fprintf('Part Two: Distance to the first location visited twice is %d blocks.\n', distance_to_first_revisit);
end

function instructions = read_instructions(filename)
    % Reads a comma-separated string of instructions from a file.
    % Returns a cell array of strings, where each string is a single instruction.
    try
        fid = fopen(filename, 'r');
        if fid == -1
            error('Could not open file: %s', filename);
        end
        line = fgetl(fid);
        fclose(fid);
        if line == -1
            instructions = {}; % Empty file
        else
            instructions = strsplit(line, ', ');
        end
    catch ME
        fprintf('Error reading file: %s\n', ME.message);
        instructions = {}; % Return empty on error
    end
end

function [final_x, final_y] = calculate_final_position(instructions)
    % Calculates the final (x, y) coordinates after following a sequence of instructions.
    %
    % Args:
    %   instructions: A cell array of strings, where each string is an instruction
    %                 (e.g., 'R2', 'L3').
    %
    % Returns:
    %   final_x: The final x-coordinate.
    %   final_y: The final y-coordinate.

    x = 0; % Current x-coordinate
    y = 0; % Current y-coordinate
    direction = 0; % 0: North, 1: East, 2: South, 3: West

    for i = 1:length(instructions)
        instruction = instructions{i};
        turn = instruction(1);
        steps = str2double(instruction(2:end));

        % Update direction based on turn
        if turn == 'R'
            direction = mod(direction + 1, 4);
        elseif turn == 'L'
            direction = mod(direction - 1 + 4, 4); % Add 4 to handle negative results of mod
        end

        % Update coordinates based on direction and steps
        switch direction
            case 0 % North
                y = y + steps;
            case 1 % East
                x = x + steps;
            case 2 % South
                y = y - steps;
            case 3 % West
                x = x - steps;
        end
    end
    final_x = x;
    final_y = y;
end

function distance = calculate_first_revisit_distance(instructions)
    % Calculates the Manhattan distance to the first location visited twice.
    %
    % Args:
    %   instructions: A cell array of strings, where each string is an instruction
    %                 (e.g., 'R2', 'L3').
    %
    % Returns:
    %   distance: The Manhattan distance to the first revisited location.
    %             Returns -1 if no location is revisited.

    x = 0; % Current x-coordinate
    y = 0; % Current y-coordinate
    direction = 0; % 0: North, 1: East, 2: South, 3: West

    % Store visited locations. Use a map for efficient lookup.
    visited_locations = containers.Map('KeyType', 'char', 'ValueType', 'logical');
    visited_locations([num2str(x) ',' num2str(y)]) = true;

    for i = 1:length(instructions)
        instruction = instructions{i};
        turn = instruction(1);
        steps = str2double(instruction(2:end));

        % Update direction based on turn
        if turn == 'R'
            direction = mod(direction + 1, 4);
        elseif turn == 'L'
            direction = mod(direction - 1 + 4, 4);
        end

        % Move step by step and check for revisits
        for step = 1:steps
            switch direction
                case 0 % North
                    y = y + 1;
                case 1 % East
                    x = x + 1;
                case 2 % South
                    y = y - 1;
                case 3 % West
                    x = x - 1;
            end

            current_location_key = [num2str(x) ',' num2str(y)];
            if isKey(visited_locations, current_location_key)
                distance = abs(x) + abs(y);
                return; % Found the first revisited location
            else
                visited_locations(current_location_key) = true;
            end
        end
    end

    distance = -1; % No location was revisited
end

% --- Entry point of the program ---
% This ensures the code runs when the script is executed.
if isfile('input.txt')
    main();
else
    fprintf('Error: input.txt not found. Please create the file with your instructions.\n');
end
