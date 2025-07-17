
function main()
    WIDTH = 7;
    NUM_ROCKS = 2022;
    CHAMBER_HEIGHT = 4000;

    fid = fopen('input.txt', 'r');
    jet_pattern = fgetl(fid);
    fclose(fid);
    jet_len = length(jet_pattern);

    rock_shapes = {
        [0 0; 1 0; 2 0; 3 0], ...
        [1 0; 0 1; 1 1; 2 1; 1 2], ...
        [0 0; 1 0; 2 0; 2 1; 2 2], ...
        [0 0; 0 1; 0 2; 0 3], ...
        [0 0; 1 0; 0 1; 1 1]
    };
    NUM_SHAPES = numel(rock_shapes);

    chamber = false(CHAMBER_HEIGHT, WIDTH);
    highest_y = 0;
    jet_idx = 1;

    for rock_count = 1:NUM_ROCKS
        shape_idx = mod(rock_count - 1, NUM_SHAPES) + 1;
        rock_pos = rock_shapes{shape_idx} + [3, highest_y + 4];

        while true
            jet = jet_pattern(jet_idx);
            jet_idx = mod(jet_idx, jet_len) + 1;
            dx = (jet == '>') - (jet == '<');

            next_pos_h = rock_pos + [dx, 0];
            if all(next_pos_h(:, 1) >= 1) && all(next_pos_h(:, 1) <= WIDTH)
                indices = sub2ind(size(chamber), next_pos_h(:, 2), next_pos_h(:, 1));
                if ~any(chamber(indices))
                    rock_pos = next_pos_h;
                end
            end

            next_pos_v = rock_pos + [0, -1];
            if any(next_pos_v(:, 2) < 1)
                break;
            end
            
            indices = sub2ind(size(chamber), next_pos_v(:, 2), next_pos_v(:, 1));
            if any(chamber(indices))
                break;
            end
            
            rock_pos = next_pos_v;
        end
        
        indices = sub2ind(size(chamber), rock_pos(:, 2), rock_pos(:, 1));
        chamber(indices) = true;
        highest_y = max(highest_y, max(rock_pos(:, 2)));
    end

    fprintf('%d\n', highest_y);
end
