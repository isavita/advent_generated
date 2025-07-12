
function solve_grid_puzzle()
    data = fileread('input.txt');
    lines = strsplit(data, '\n');
    lines = lines(3:end-1); % Skip header lines and the last empty line

    nodes = struct('used', {}, 'avail', {});
    max_x = 0;
    max_y = 0;

    for i = 1:length(lines)
        parts = regexp(lines{i}, '/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T\s+(\d+)%', 'tokens');
        if ~isempty(parts)
            x = str2double(parts{1}{1});
            y = str2double(parts{1}{2});
            used = str2double(parts{1}{4});
            avail = str2double(parts{1}{5});

            nodes(x+1, y+1).used = used;
            nodes(x+1, y+1).avail = avail;

            max_x = max(max_x, x);
            max_y = max(max_y, y);
        end
    end

    width = max_x + 1;
    height = max_y + 1;
    wall_threshold = 400;

    % Find initial hole (used == 0)
    hole_pos = [NaN, NaN];
    for y = 0:height-1
        for x = 0:width-1
            if nodes(x+1, y+1).used == 0
                hole_pos = [x, y];
                break;
            end
        end
        if ~isnan(hole_pos(1))
            break;
        end
    end

    if isnan(hole_pos(1))
        error('No hole found.');
    end

    goal_pos = [width - 1, 0];
    moves_sum = 0;

    while ~isequal(goal_pos, [0, 0])
        hole_target_pos = [goal_pos(1) - 1, goal_pos(2)];

        % BFS to find moves for the hole
        queue = [hole_pos];
        visited = false(width, height);
        visited(hole_pos(1)+1, hole_pos(2)+1) = true;
        distance = -ones(width, height);
        distance(hole_pos(1)+1, hole_pos(2)+1) = 0;

        head = 1;
        found_path = false;

        while head <= size(queue, 1)
            current_pos = queue(head, :);
            head = head + 1;

            if isequal(current_pos, hole_target_pos)
                moves_sum = moves_sum + distance(current_pos(1)+1, current_pos(2)+1);
                hole_pos = hole_target_pos;
                found_path = true;
                break;
            end

            neighbors = [0, 1; 0, -1; 1, 0; -1, 0];
            for k = 1:4
                next_pos = current_pos + neighbors(k, :);

                if next_pos(1) < 0 || next_pos(2) < 0 || next_pos(1) >= width || next_pos(2) >= height
                    continue;
                end
                if isequal(next_pos, goal_pos) % Cannot move hole into goal data
                    continue;
                end
                if nodes(next_pos(1)+1, next_pos(2)+1).used > wall_threshold
                    continue;
                end
                if ~visited(next_pos(1)+1, next_pos(2)+1)
                    visited(next_pos(1)+1, next_pos(2)+1) = true;
                    distance(next_pos(1)+1, next_pos(2)+1) = distance(current_pos(1)+1, current_pos(2)+1) + 1;
                    queue = [queue; next_pos];
                end
            end
        end

        if ~found_path
            error('No path found for the hole.');
        end

        % Move the goal data into the hole (1 move)
        moves_sum = moves_sum + 1;

        % Swap goal and hole positions
        temp_goal = goal_pos;
        goal_pos = hole_pos;
        hole_pos = temp_goal;
    end

    fprintf('%d\n', moves_sum);
end
