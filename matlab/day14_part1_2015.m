
function reindeer_olympics()
    % Reindeer Olympics - Day 14
    % Reads reindeer data from input.txt and calculates the winning distance
    % after a specified race duration.

    race_duration = 2503; % Seconds

    % Read reindeer data from input.txt
    reindeer_data = read_reindeer_data('input.txt');

    % Initialize variables to track the maximum distance and points
    max_distance = 0;
    reindeer_points = containers.Map('KeyType', 'char', 'ValueType', 'double');
    reindeer_distances = containers.Map('KeyType', 'char', 'ValueType', 'double');

    % Initialize points for each reindeer
    reindeer_names = keys(reindeer_data);
    for i = 1:length(reindeer_names)
        reindeer_points(reindeer_names{i}) = 0;
        reindeer_distances(reindeer_names{i}) = 0;
    end

    % Simulate the race second by second
    for current_second = 1:race_duration
        current_max_distance_this_second = 0;

        % Update each reindeer's position
        for i = 1:length(reindeer_names)
            name = reindeer_names{i};
            data = reindeer_data(name);
            speed = data.speed;
            fly_duration = data.fly_duration;
            rest_duration = data.rest_duration;

            % Calculate time spent flying and resting within the current second
            cycle_time = fly_duration + rest_duration;
            time_in_cycle = mod(current_second - 1, cycle_time);

            if time_in_cycle < fly_duration
                % Reindeer is flying
                reindeer_distances(name) = reindeer_distances(name) + speed;
            end

            % Track the maximum distance achieved in this second for scoring
            if reindeer_distances(name) > current_max_distance_this_second
                current_max_distance_this_second = reindeer_distances(name);
            end
        end

        % Award points to reindeer that are currently in the lead
        for i = 1:length(reindeer_names)
            name = reindeer_names{i};
            if reindeer_distances(name) == current_max_distance_this_second
                reindeer_points(name) = reindeer_points(name) + 1;
            end
        end

        % Update the overall maximum distance
        if current_max_distance_this_second > max_distance
            max_distance = current_max_distance_this_second;
        end
    end

    % Find the reindeer with the most points
    max_points = 0;
    winning_reindeer = '';
    point_keys = keys(reindeer_points);
    for i = 1:length(point_keys)
        name = point_keys{i};
        if reindeer_points(name) > max_points
            max_points = reindeer_points(name);
            winning_reindeer = name;
        end
    end

    % Print the results to standard output
    fprintf('After %d seconds:\n', race_duration);
    fprintf('The winning reindeer is: %s\n', winning_reindeer);
    fprintf('With a total distance of: %.0f km\n', max_distance);
    fprintf('And a total of %d points.\n', max_points);

    % Optional: Print distances and points for all reindeer
    fprintf('\n--- Detailed Results ---\n');
    for i = 1:length(reindeer_names)
        name = reindeer_names{i};
        fprintf('%s: Distance = %.0f km, Points = %d\n', name, reindeer_distances(name), reindeer_points(name));
    end
end

function reindeer_data = read_reindeer_data(filename)
    % Reads reindeer data from the specified file.
    % Expected format: "Name can fly Speed km/s for Duration seconds, but then must rest for RestDuration seconds."

    reindeer_data = containers.Map('KeyType', 'char', 'ValueType', 'any');
    fid = fopen(filename, 'r');
    if fid == -1
        error('Could not open file: %s', filename);
    end

    tline = fgetl(fid);
    while ischar(tline)
        % Use regular expressions to extract information
        % Pattern: "(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds."
        match = regexp(tline, '(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.', 'tokens');

        if ~isempty(match)
            name = match{1}{1};
            speed = str2double(match{1}{2});
            fly_duration = str2double(match{1}{3});
            rest_duration = str2double(match{1}{4});

            reindeer_data(name) = struct('speed', speed, 'fly_duration', fly_duration, 'rest_duration', rest_duration);
        end
        tline = fgetl(fid);
    end
    fclose(fid);
end

% To run this program, save it as reindeer_olympics.m and create an input.txt file
% with the reindeer descriptions. Then, in the MATLAB command window, type:
% reindeer_olympics
