
function solve()
    filename = 'input.txt';
    fid = fopen(filename, 'r');
    lines = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    lines = lines{1};
    initial_rows = length(lines);
    initial_cols = length(lines{1});
    initialGrid = zeros(initial_rows, initial_cols);
    for i = 1:initial_rows
        initialGrid(i, :) = arrayfun(@(c) str2double(c), char(lines{i}));
    end

    EXTENDED_SIZE = 5;
    extended_rows = initial_rows * EXTENDED_SIZE;
    extended_cols = initial_cols * EXTENDED_SIZE;
    extendedGrid = zeros(extended_rows, extended_cols);
    for i = 0:(extended_rows - 1)
        for j = 0:(extended_cols - 1)
            original_risk = initialGrid(mod(i, initial_rows) + 1, mod(j, initial_cols) + 1);
            new_risk = original_risk + floor(i / initial_rows) + floor(j / initial_cols);
            while new_risk > 9
                new_risk = new_risk - 9;
            end
            extendedGrid(i + 1, j + 1) = new_risk;
        end
    end

    dist = inf(extended_rows, extended_cols);
    dist(1, 1) = 0;

    heap_capacity = extended_rows * extended_cols;
    heap = zeros(heap_capacity, 3);
    heap_size = 0;

    function push_heap(element)
        heap_size = heap_size + 1;
        heap(heap_size, :) = element;
        idx = heap_size;
        while idx > 1 && heap(idx, 1) < heap(floor(idx/2), 1)
            swap_heap(idx, floor(idx/2));
            idx = floor(idx/2);
        end
    end

    function element = pop_heap()
        element = heap(1, :);
        heap(1, :) = heap(heap_size, :);
        heap_size = heap_size - 1;
        idx = 1;
        while true
            left_child = 2 * idx;
            right_child = 2 * idx + 1;
            min_idx = idx;

            if left_child <= heap_size && heap(left_child, 1) < heap(min_idx, 1)
                min_idx = left_child;
            end
            if right_child <= heap_size && heap(right_child, 1) < heap(min_idx, 1)
                min_idx = right_child;
            end

            if min_idx ~= idx
                swap_heap(idx, min_idx);
                idx = min_idx;
            else
                break;
            end
        end
    end

    function swap_heap(idx1, idx2)
        temp = heap(idx1, :);
        heap(idx1, :) = heap(idx2, :);
        heap(idx2, :) = temp;
    end

    push_heap([0, 1, 1]);

    dx = [0, 1, 0, -1];
    dy = [1, 0, -1, 0];

    while heap_size > 0
        curr = pop_heap();
        curr_risk = curr(1);
        curr_x = curr(2);
        curr_y = curr(3);

        if curr_risk > dist(curr_x, curr_y)
            continue;
        end

        if curr_x == extended_rows && curr_y == extended_cols
            fprintf('%d\n', curr_risk);
            return;
        end

        for i = 1:4
            nx = curr_x + dx(i);
            ny = curr_y + dy(i);

            if nx >= 1 && nx <= extended_rows && ny >= 1 && ny <= extended_cols
                next_risk = curr_risk + extendedGrid(nx, ny);
                if next_risk < dist(nx, ny)
                    dist(nx, ny) = next_risk;
                    push_heap([next_risk, nx, ny]);
                end
            end
        end
    end
    fprintf('%d\n', -1);
end
