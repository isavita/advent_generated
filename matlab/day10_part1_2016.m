
function main()
    solve();
end

function solve()
    % Initialize a containers.Map to store bot information.
    % Each value in the map will be a struct with fields:
    % 'lowTo': string ID of the target for the low chip (e.g., 'bot 10', 'output 0')
    % 'highTo': string ID of the target for the high chip
    % 'chips': numeric array of chips currently held by the bot
    botsMap = containers.Map;

    % Open the input file
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Could not open input.txt');
    end

    % Regular expressions for parsing input lines
    value_regex = 'value (\d+) goes to (bot \d+)';
    gives_regex = '(bot \d+) gives low to (bot \d+|output \d+) and high to (bot \d+|output \d+)';

    % Read and parse input lines
    while ~feof(fid)
        line = fgetl(fid);
        if ~ischar(line), break; end % End of file or error

        % Try to match 'value' line
        value_match = regexp(line, value_regex, 'tokens', 'once');
        if ~isempty(value_match)
            value = str2double(value_match{1});
            bot_id = value_match{2};
            
            % Get or create bot entry and update its chips
            current_bot = get_or_create_bot(botsMap, bot_id);
            current_bot.chips = [current_bot.chips, value];
            botsMap(bot_id) = current_bot; % Update the map with the modified struct

        % Try to match 'gives' line
        else
            gives_match = regexp(line, gives_regex, 'tokens', 'once');
            if ~isempty(gives_match)
                bot_id = gives_match{1};
                low_to = gives_match{2};
                high_to = gives_match{3};
                
                % Get or create bot entry and update its rules
                current_bot = get_or_create_bot(botsMap, bot_id);
                current_bot.lowTo = low_to;
                current_bot.highTo = high_to;
                botsMap(bot_id) = current_bot; % Update the map with the modified struct
            end
        end
    end
    fclose(fid);

    % Simulation loop
    while true
        action_performed = false;
        bot_ids = keys(botsMap); % Get all bot IDs (keys) for current iteration

        % Iterate through all bots.
        % Note: Iterating over a snapshot of keys is safe even if the map
        % is modified (new bots added) during the loop. New bots will be
        % processed in subsequent iterations if they receive chips.
        for i = 1:length(bot_ids)
            bot_id = bot_ids{i};
            
            % Retrieve the bot struct from the map. 'b' is a copy.
            b = botsMap(bot_id); 

            if length(b.chips) == 2 % If the bot has two chips
                action_performed = true;

                % Sort chips to find low and high
                sorted_chips = sort(b.chips);
                low = sorted_chips(1);
                high = sorted_chips(2);

                % Check for the target condition (chips 17 and 61)
                if low == 17 && high == 61
                    fprintf('%s\n', bot_id);
                    return; % Exit the function (and thus the program)
                end

                % Clear the bot's chips
                b.chips = [];
                botsMap(bot_id) = b; % Update the map with the cleared chips

                % Distribute chips to their destinations
                if ~isempty(b.lowTo)
                    target_id = b.lowTo;
                    target_bot = get_or_create_bot(botsMap, target_id);
                    target_bot.chips = [target_bot.chips, low];
                    botsMap(target_id) = target_bot; % Update target in map
                end

                if ~isempty(b.highTo)
                    target_id = b.highTo;
                    target_bot = get_or_create_bot(botsMap, target_id);
                    target_bot.chips = [target_bot.chips, high];
                    botsMap(target_id) = target_bot; % Update target in map
                end
            end
        end

        % If no action was performed in this iteration, break the loop
        if ~action_performed
            break;
        end
    end
end

% Helper function to get a bot struct from the map, or create a new one if it doesn't exist.
% This function modifies the botsMap by adding a new entry if 'bot_id' is not found.
% It returns a *copy* of the struct, so the caller must put the modified struct back into the map.
function bot_struct = get_or_create_bot(botsMap, bot_id)
    if botsMap.isKey(bot_id)
        bot_struct = botsMap(bot_id);
    else
        % Initialize a new bot struct
        bot_struct = struct('lowTo', '', 'highTo', '', 'chips', []);
        botsMap(bot_id) = bot_struct; % Add the newly created struct to the map
    end
end
