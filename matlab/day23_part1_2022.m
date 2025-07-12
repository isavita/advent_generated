
function main()
    ROUNDS = 10;
    adj_dx = [-1, 0, 1, -1, 1, -1, 0, 1];
    adj_dy = [-1, -1, -1, 0, 0, 1, 1, 1];

    checks = {
        [-1, -1; 0, -1; 1, -1],
        [-1,  1; 0,  1; 1,  1],
        [-1, -1; -1, 0; -1, 1],
        [ 1, -1;  1, 0;  1, 1]
    };
    moves = [
        0, -1;
        0,  1;
        -1, 0;
        1, 0
    ];

    elves = containers.Map();

    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Error opening input.txt');
    end

    y = 0;
    while ~feof(fid)
        line = fgetl(fid);
        if ischar(line)
            for x = 1:length(line)
                if line(x) == '#'
                    key = sprintf('%d_%d', x-1, y);
                    elves(key) = true;
                end
            end
        end
        y = y + 1;
    end
    fclose(fid);

    dir_order = [1, 2, 3, 4];

    for round = 1:ROUNDS
        proposals = containers.Map();
        
        elf_keys = keys(elves);
        current_elves_coords = zeros(length(elf_keys), 2);
        for i = 1:length(elf_keys)
            parts = sscanf(elf_keys{i}, '%d_%d');
            current_elves_coords(i, :) = parts';
        end

        for i = 1:size(current_elves_coords, 1)
            elf_x = current_elves_coords(i, 1);
            elf_y = current_elves_coords(i, 2);

            has_neighbor = false;
            for j = 1:8
                neighbor_x = elf_x + adj_dx(j);
                neighbor_y = elf_y + adj_dy(j);
                if isKey(elves, sprintf('%d_%d', neighbor_x, neighbor_y))
                    has_neighbor = true;
                    break;
                end
            end

            if ~has_neighbor
                continue;
            end

            for k_idx = 1:4
                dir_idx = dir_order(k_idx);
                can_move = true;
                
                current_checks = checks{dir_idx};
                for check_row = 1:size(current_checks, 1)
                    check_dx = current_checks(check_row, 1);
                    check_dy = current_checks(check_row, 2);
                    check_pos_x = elf_x + check_dx;
                    check_pos_y = elf_y + check_dy;
                    if isKey(elves, sprintf('%d_%d', check_pos_x, check_pos_y))
                        can_move = false;
                        break;
                    end
                end

                if can_move
                    move_dx = moves(dir_idx, 1);
                    move_dy = moves(dir_idx, 2);
                    dest_x = elf_x + move_dx;
                    dest_y = elf_y + move_dy;
                    dest_key = sprintf('%d_%d', dest_x, dest_y);

                    if isKey(proposals, dest_key)
                        info = proposals(dest_key);
                        info.count = info.count + 1;
                        proposals(dest_key) = info;
                    else
                        proposals(dest_key) = struct('count', 1, 'first_proposer', [elf_x, elf_y]);
                    end
                    break;
                end
            end
        end

        moves_made = 0;
        proposal_keys = keys(proposals);
        for i = 1:length(proposal_keys)
            dest_key = proposal_keys{i};
            info = proposals(dest_key);

            if info.count == 1
                proposer_x = info.first_proposer(1);
                proposer_y = info.first_proposer(2);
                proposer_key = sprintf('%d_%d', proposer_x, proposer_y);

                if isKey(elves, proposer_key)
                    remove(elves, proposer_key);
                    elves(dest_key) = true;
                    moves_made = moves_made + 1;
                end
            end
        end

        first_dir = dir_order(1);
        dir_order(1:3) = dir_order(2:4);
        dir_order(4) = first_dir;
    end

    if elves.Count == 0
        fprintf('0\n');
        return;
    end

    min_x = inf; max_x = -inf;
    min_y = inf; max_y = -inf;

    elf_keys = keys(elves);
    for i = 1:length(elf_keys)
        parts = sscanf(elf_keys{i}, '%d_%d');
        elf_x = parts(1);
        elf_y = parts(2);

        min_x = min(min_x, elf_x);
        max_x = max(max_x, elf_x);
        min_y = min(min_y, elf_y);
        max_y = max(max_y, elf_y);
    end

    width = max_x - min_x + 1;
    height = max_y - min_y + 1;
    total_tiles = width * height;
    empty_tiles = total_tiles - elves.Count;

    fprintf('%d\n', empty_tiles);

end
