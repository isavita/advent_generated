
function main()
    % Read blocked IP ranges from input.txt
    blocked_ranges = read_blocked_ranges('input.txt');

    % Sort and merge overlapping ranges
    merged_ranges = merge_ranges(blocked_ranges);

    % Part One: Find the lowest allowed IP
    lowest_allowed_ip = find_lowest_allowed_ip(merged_ranges);
    fprintf('Lowest allowed IP: %d\n', lowest_allowed_ip);

    % Part Two: Count the number of allowed IPs
    total_allowed_ips = count_allowed_ips(merged_ranges);
    fprintf('Total allowed IPs: %d\n', total_allowed_ips);
end

function blocked_ranges = read_blocked_ranges(filename)
    % Reads IP ranges from a file.
    % Each line in the file is expected to be in the format "start-end".
    % Returns a 2D array where each row is [start, end].
    try
        fid = fopen(filename, 'r');
        if fid == -1
            error('Could not open file: %s', filename);
        end

        lines = textscan(fid, '%s', 'Delimiter', '\n');
        fclose(fid);

        blocked_ranges = zeros(length(lines{1}), 2, 'uint32'); % Use uint32 for IP addresses
        for i = 1:length(lines{1})
            parts = strsplit(lines{1}{i}, '-');
            blocked_ranges(i, 1) = str2double(parts{1});
            blocked_ranges(i, 2) = str2double(parts{2});
        end
    catch ME
        fprintf('Error reading file: %s\n', ME.message);
        blocked_ranges = []; % Return empty if error
    end
end

function merged_ranges = merge_ranges(ranges)
    % Merges overlapping and adjacent IP ranges.
    % Input: A 2D array where each row is [start, end].
    % Output: A 2D array of merged, non-overlapping ranges.

    if isempty(ranges)
        merged_ranges = [];
        return;
    end

    % Sort ranges by their start IP
    ranges = sortrows(ranges, 1);

    merged_ranges = zeros(0, 2, 'uint32');
    current_start = ranges(1, 1);
    current_end = ranges(1, 2);

    for i = 2:size(ranges, 1)
        next_start = ranges(i, 1);
        next_end = ranges(i, 2);

        % Check for overlap or adjacency
        if next_start <= current_end + 1
            % Merge the ranges
            current_end = max(current_end, next_end);
        else
            % No overlap, add the current merged range and start a new one
            merged_ranges = [merged_ranges; current_start, current_end];
            current_start = next_start;
            current_end = next_end;
        end
    end

    % Add the last merged range
    merged_ranges = [merged_ranges; current_start, current_end];
end

function lowest_ip = find_lowest_allowed_ip(merged_ranges)
    % Finds the lowest IP address that is not blocked.
    % Assumes the maximum possible IP is 4294967295 (2^32 - 1).
    % Input: A 2D array of merged, non-overlapping blocked ranges.
    % Output: The lowest allowed IP address.

    MAX_IP = uint32(4294967295);

    if isempty(merged_ranges)
        lowest_ip = uint32(0); % If no ranges are blocked, 0 is allowed
        return;
    end

    % Check if 0 is blocked
    if merged_ranges(1, 1) > 0
        lowest_ip = uint32(0);
        return;
    end

    % Iterate through the merged ranges to find the first gap
    for i = 1:size(merged_ranges, 1)
        % The next allowed IP would be one greater than the current range's end
        potential_ip = merged_ranges(i, 2) + 1;

        % If this potential IP is within the valid range and not blocked by the next range
        if potential_ip <= MAX_IP
            if i + 1 <= size(merged_ranges, 1)
                if potential_ip < merged_ranges(i + 1, 1)
                    lowest_ip = potential_ip;
                    return;
                end
            else
                % This is the last range, so any IP after its end is allowed
                lowest_ip = potential_ip;
                return;
            end
        end
    end

    % If all IPs up to MAX_IP are covered by blocked ranges, this case should ideally not happen
    % for a valid puzzle input where there's at least one allowed IP.
    % However, as a fallback, we can indicate no allowed IP found or return MAX_IP + 1 (which is invalid).
    % For this problem, we expect an allowed IP.
    lowest_ip = MAX_IP + 1; % Indicate no allowed IP found within the valid range
end

function count = count_allowed_ips(merged_ranges)
    % Counts the total number of allowed IP addresses.
    % Input: A 2D array of merged, non-overlapping blocked ranges.
    % Output: The total count of allowed IPs.

    MAX_IP = uint32(4294967295);
    total_ips_in_range = uint64(MAX_IP) + 1; % Total possible IPs from 0 to MAX_IP

    if isempty(merged_ranges)
        count = total_ips_in_range; % All IPs are allowed
        return;
    end

    blocked_count = uint64(0);
    for i = 1:size(merged_ranges, 1)
        % Ensure ranges don't exceed MAX_IP
        start = max(uint32(0), merged_ranges(i, 1));
        end_ip = min(MAX_IP, merged_ranges(i, 2));

        if start <= end_ip
            blocked_count = blocked_count + uint64(end_ip - start + 1);
        end
    end

    count = total_ips_in_range - blocked_count;
end

% To run the program, save this code as a .m file (e.g., firewall_solver.m)
% and then call the main function from the MATLAB command window:
% firewall_solver()
