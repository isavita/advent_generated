
function trick_shot()
    % Reads target area from input.txt and calculates the highest y position
    % and the total number of distinct initial velocities that hit the target.

    % Read target area from input.txt
    try
        fid = fopen('input.txt', 'r');
        if fid == -1
            error('Could not open input.txt for reading.');
        end
        line = fgetl(fid);
        fclose(fid);
    catch ME
        fprintf('Error reading input file: %s\n', ME.message);
        return;
    end

    % Parse the target area string
    % Example: "target area: x=20..30, y=-10..-5"
    tokens = regexp(line, 'x=(\d+)..(\d+), y=(-?\d+)..(-?\d+)', 'tokens');
    if isempty(tokens)
        error('Could not parse target area from input file. Ensure format is "target area: x=X1..X2, y=Y1..Y2".');
    end

    x_min = str2double(tokens{1}{1});
    x_max = str2double(tokens{1}{2});
    y_min = str2double(tokens{1}{3});
    y_max = str2double(tokens{1}{4});

    % --- Part 1: Find the highest y position ---
    % The highest y position is achieved when the initial y velocity is such
    % that the probe reaches its apex and then starts to descend, hitting
    % the target area. The maximum possible initial y velocity that can
    % reach the target area without overshooting on the first step is
    % related to the minimum y coordinate of the target area.
    % If y_min is negative, the probe must have a positive initial y velocity
    % to reach it. The highest it can go is when it reaches its peak and then
    % falls. The peak height is achieved when the y velocity becomes 0.
    % The sum of velocities from v to 1 is v*(v+1)/2.
    % So, if the peak is at height H, then H = initial_vy * (initial_vy + 1) / 2.
    % To hit the target area, the probe must eventually reach a y position
    % between y_min and y_max. The highest possible y position will occur
    % when the initial y velocity is such that the probe reaches its apex
    % and then starts to fall, eventually landing in the target area.
    % The maximum initial y velocity that can hit the target area is related
    % to the lowest y coordinate of the target area. If the probe starts with
    % an initial y velocity of `v_y`, its highest point will be `v_y * (v_y + 1) / 2`.
    % To ensure it can eventually reach the target area, this peak height
    % must be at least `y_min`.
    % The optimal initial `v_y` for maximum height is `abs(y_min) - 1`.
    % This is because if `v_y = abs(y_min) - 1`, the probe will reach its peak
    % at `(abs(y_min) - 1) * abs(y_min) / 2`. After this, its velocity will
    % decrease. When its y velocity becomes `-abs(y_min)`, it will be at
    % `y = (abs(y_min) - 1) * abs(y_min) / 2 - abs(y_min)`.
    % If the target area's lowest point is `y_min`, then the probe must
    % eventually reach a y position less than or equal to `y_min`.
    % The highest possible y position is achieved when the initial y velocity
    % is `abs(y_min) - 1`.
    max_initial_vy_for_part1 = abs(y_min) - 1;
    highest_y_position = max_initial_vy_for_part1 * (max_initial_vy_for_part1 + 1) / 2;

    fprintf('Part 1: Highest y position reached: %d\n', highest_y_position);

    % --- Part 2: Count distinct initial velocities ---
    % We need to find all initial (vx, vy) pairs that result in the probe
    % hitting the target area.
    %
    % Bounds for initial velocities:
    % vx:
    %   - Minimum vx: If vx is 0 or negative, the probe will never move forward.
    %     To reach a positive x_min, vx must be at least 1.
    %   - Maximum vx: If vx is too large, the probe will overshoot the target
    %     area on the first step. The maximum vx can be x_max. If vx > x_max,
    %     and x_min > 0, it's possible to overshoot. A safe upper bound is x_max.
    % vy:
    %   - Minimum vy: If vy is too small (very negative), the probe will fall
    %     too quickly and miss the target area. The lowest possible vy to
    %     eventually reach y_min is when the probe's y velocity becomes
    %     `y_min` on the first step, meaning `initial_vy = y_min`.
    %   - Maximum vy: If vy is too large, the probe will overshoot the target
    %     area on the first step. The highest possible vy that can still
    %     eventually hit the target area is `abs(y_min) - 1` (as determined
    %     in Part 1). If `vy > abs(y_min) - 1`, the probe will reach its peak
    %     and then start descending. If the peak is too high, it might
    %     overshoot the target area's upper bound. A safe upper bound is
    %     `abs(y_min) - 1`.

    % Determine reasonable search ranges for vx and vy.
    % For vx:
    %   - Minimum vx: 1 (to move forward)
    %   - Maximum vx: x_max (if vx > x_max, it will likely overshoot on the first step if x_min > 0)
    % For vy:
    %   - Minimum vy: y_min (if vy < y_min, it will fall too fast and miss)
    %   - Maximum vy: abs(y_min) - 1 (as determined in Part 1 for max height)

    vx_min_search = 1;
    vx_max_search = x_max; % A reasonable upper bound, can be refined if needed.
    vy_min_search = y_min;
    vy_max_search = abs(y_min) - 1; % The highest possible initial vy that can reach the target.

    valid_velocities = 0;
    % Use a set to store unique velocity pairs to avoid double counting.
    % MATLAB doesn't have a built-in set for pairs, so we can use a cell array
    % and check for existence, or simply count and rely on the loop structure.
    % For simplicity and clarity, we'll just count.

    for initial_vx = vx_min_search:vx_max_search
        for initial_vy = vy_min_search:vy_max_search
            % Simulate the probe's trajectory for this initial velocity
            current_x = 0;
            current_y = 0;
            vx = initial_vx;
            vy = initial_vy;
            hit_target = false;
            overshot = false;

            % Simulate steps until the probe is out of bounds or hits the target
            while ~overshot
                current_x = current_x + vx;
                current_y = current_y + vy;

                % Check if the probe is within the target area
                if current_x >= x_min && current_x <= x_max && current_y >= y_min && current_y <= y_max
                    hit_target = true;
                    break; % Probe hit the target, no need to simulate further for this velocity
                end

                % Update velocities
                if vx > 0
                    vx = vx - 1;
                elseif vx < 0
                    vx = vx + 1;
                end
                vy = vy - 1;

                % Check for conditions that mean the probe will never hit the target
                % If the probe has passed the target area horizontally and is moving away, or
                % if it has fallen below the target area and is moving further down.
                if (vx == 0 && current_x < x_min) || ... % Stopped moving horizontally before reaching target
                   (vx > 0 && current_x > x_max) || ... % Moving right and already past target
                   (vx < 0 && current_x < x_min) || ... % Moving left and already past target
                   (current_y < y_min && vy < 0)      % Falling below target and continuing to fall
                    overshot = true;
                end
            end

            if hit_target
                valid_velocities = valid_velocities + 1;
            end
        end
    end

    fprintf('Part 2: Number of distinct initial velocities: %d\n', valid_velocities);
end

% Helper function to simulate a single trajectory (optional, for debugging)
% function [hit, max_y] = simulate_trajectory(initial_vx, initial_vy, x_min, x_max, y_min, y_max)
%     current_x = 0;
%     current_y = 0;
%     vx = initial_vx;
%     vy = initial_vy;
%     hit = false;
%     max_y_reached = 0;
%
%     while true
%         current_x = current_x + vx;
%         current_y = current_y + vy;
%
%         max_y_reached = max(max_y_reached, current_y);
%
%         % Check if within target area
%         if current_x >= x_min && current_x <= x_max && current_y >= y_min && current_y <= y_max
%             hit = true;
%             break;
%         end
%
%         % Check if out of bounds (will never hit target)
%         if (current_x > x_max && vx >= 0) || (current_x < x_min && vx <= 0) || (current_y < y_min && vy < 0)
%             break;
%         end
%
%         % Update velocities
%         if vx > 0
%             vx = vx - 1;
%         elseif vx < 0
%             vx = vx + 1;
%         end
%         vy = vy - 1;
%     end
%     max_y = max_y_reached;
% end

