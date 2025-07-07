
function main()
    % Read input from input.txt
    try
        fid = fopen('input.txt', 'r');
        if fid == -1
            error('Could not open input.txt for reading.');
        end
        
        % Read the first line (Time)
        time_line = fgetl(fid);
        % Read the second line (Distance)
        distance_line = fgetl(fid);
        
        fclose(fid);
    catch ME
        fprintf(2, 'Error reading input file: %s\n', ME.message);
        return;
    end

    % --- Part 1: Process individual races ---
    
    % Extract numbers from the time line
    times_str = regexp(time_line, '\d+', 'match');
    times = cellfun(@str2double, times_str);
    
    % Extract numbers from the distance line
    distances_str = regexp(distance_line, '\d+', 'match');
    distances = cellfun(@str2double, distances_str);

    % Calculate the product of ways to win for each race
    product_of_ways_part1 = 1;
    for i = 1:length(times)
        race_time = times(i);
        record_distance = distances(i);
        
        ways_to_win = count_ways_to_win(race_time, record_distance);
        product_of_ways_part1 = product_of_ways_part1 * ways_to_win;
    end

    fprintf('Part 1: Product of ways to win: %d\n', product_of_ways_part1);

    % --- Part 2: Process as a single race ---
    
    % Concatenate numbers to form a single large number
    single_time_str = strjoin(times_str, '');
    single_distance_str = strjoin(distances_str, '');
    
    single_race_time = str2double(single_time_str);
    single_record_distance = str2double(single_distance_str);
    
    ways_to_win_part2 = count_ways_to_win(single_race_time, single_record_distance);
    
    fprintf('Part 2: Ways to win for the single race: %d\n', ways_to_win_part2);

end

function ways = count_ways_to_win(total_time, record_distance)
    % Calculates the number of ways to beat the record distance.
    %
    % For a given total_time, if the button is held for 'hold_time' milliseconds,
    % the boat moves for (total_time - hold_time) milliseconds at a speed of
    % hold_time millimeters per millisecond.
    %
    % Distance = hold_time * (total_time - hold_time)
    % We need to find the number of hold_time values (from 0 to total_time)
    % such that:
    % hold_time * (total_time - hold_time) > record_distance
    %
    % This is a quadratic inequality:
    % -hold_time^2 + total_time * hold_time - record_distance > 0
    %
    % We can find the roots of the quadratic equation:
    % -x^2 + total_time * x - record_distance = 0
    % x^2 - total_time * x + record_distance = 0
    %
    % Using the quadratic formula: x = [-b ± sqrt(b^2 - 4ac)] / 2a
    % Here, a=1, b=-total_time, c=record_distance
    % x = [total_time ± sqrt(total_time^2 - 4 * record_distance)] / 2
    
    % Calculate the discriminant
    discriminant = total_time^2 - 4 * record_distance;
    
    % If discriminant is negative, there are no real roots, meaning no way to win.
    if discriminant < 0
        ways = 0;
        return;
    end
    
    % Calculate the two roots
    root1 = (total_time - sqrt(discriminant)) / 2;
    root2 = (total_time + sqrt(discriminant)) / 2;
    
    % The number of ways to win is the number of integers 'hold_time'
    % between root1 and root2 (exclusive of the roots themselves if they are integers).
    %
    % We need hold_time > root1 and hold_time < root2.
    %
    % The smallest integer hold_time that wins is floor(root1) + 1.
    % The largest integer hold_time that wins is ceil(root2) - 1.
    %
    % However, a simpler way is to consider the range of hold_time from 0 to total_time.
    % We can iterate or use the roots.
    %
    % The number of integers between root1 and root2 (exclusive) is:
    % floor(root2 - 1) - ceil(root1 + 1) + 1
    %
    % A more direct calculation:
    % The smallest integer hold_time that satisfies the inequality is floor(root1) + 1.
    % The largest integer hold_time that satisfies the inequality is ceil(root2) - 1.
    %
    % The number of ways is (ceil(root2) - 1) - (floor(root1) + 1) + 1
    % which simplifies to ceil(root2) - floor(root1) - 1.
    %
    % Let's verify with the example:
    % Race 1: Time=7, Distance=9
    % x^2 - 7x + 9 = 0
    % discriminant = 49 - 4*9 = 49 - 36 = 13
    % root1 = (7 - sqrt(13)) / 2 = (7 - 3.605) / 2 = 3.395 / 2 = 1.6975
    % root2 = (7 + sqrt(13)) / 2 = (7 + 3.605) / 2 = 10.605 / 2 = 5.3025
    %
    % Smallest integer hold_time > 1.6975 is 2.
    % Largest integer hold_time < 5.3025 is 5.
    % Ways = 5 - 2 + 1 = 4.
    %
    % Using ceil(root2) - floor(root1) - 1:
    % ceil(5.3025) - floor(1.6975) - 1 = 6 - 1 - 1 = 4. This matches.
    
    % Ensure we only consider valid hold times (0 to total_time)
    % The roots will naturally fall within this range if a solution exists.
    
    % The number of winning hold times is the number of integers 'h' such that
    % root1 < h < root2.
    % The smallest integer greater than root1 is floor(root1) + 1.
    % The largest integer less than root2 is ceil(root2) - 1.
    %
    % Number of ways = (ceil(root2) - 1) - (floor(root1) + 1) + 1
    %                = ceil(root2) - floor(root1) - 1
    
    % To avoid floating point issues with exact roots, we can use floor/ceil carefully.
    % The smallest integer hold_time that wins is floor(root1) + 1.
    % The largest integer hold_time that wins is ceil(root2) - 1.
    
    % Let's use a more robust approach by finding the first and last winning hold times.
    
    % Find the first winning hold time
    first_winning_hold = floor(root1) + 1;
    
    % Find the last winning hold time
    last_winning_hold = ceil(root2) - 1;
    
    % Ensure these are within the valid range [0, total_time]
    first_winning_hold = max(0, first_winning_hold);
    last_winning_hold = min(total_time, last_winning_hold);
    
    % The number of ways is the count of integers from first_winning_hold to last_winning_hold
    if first_winning_hold <= last_winning_hold
        ways = last_winning_hold - first_winning_hold + 1;
    else
        ways = 0; % No winning hold times
    end
    
    % Alternative, more direct calculation using the roots:
    % The number of integers strictly between root1 and root2.
    % This is equivalent to the number of integers 'h' such that
    % h >= floor(root1) + 1 and h <= ceil(root2) - 1.
    %
    % The number of integers in [A, B] is B - A + 1.
    % So, ways = (ceil(root2) - 1) - (floor(root1) + 1) + 1
    %          = ceil(root2) - floor(root1) - 1
    %
    % Let's re-evaluate the example:
    % root1 = 1.6975, root2 = 5.3025
    % ceil(root2) = 6, floor(root1) = 1
    % ways = 6 - 1 - 1 = 4. This is correct.
    
    % Using this direct calculation:
    ways_direct = ceil(root2) - floor(root1) - 1;
    
    % Ensure the result is non-negative
    ways = max(0, ways_direct);
    
end

% To run this program:
% 1. Save the code as a .m file (e.g., boat_races.m).
% 2. Create a file named 'input.txt' in the same directory.
% 3. Populate 'input.txt' with the race data, for example:
%    Time:      7  15   30
%    Distance:  9  40  200
% 4. Open MATLAB, navigate to the directory where you saved the files.
% 5. Run the program by typing 'boat_races' (or the name of your file) in the Command Window and pressing Enter.
