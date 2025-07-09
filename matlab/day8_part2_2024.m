
function solve()
    fid = fopen('input.txt', 'r');
    grid_lines = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    grid_lines = grid_lines{1};

    h = numel(grid_lines);
    if h == 0
        fprintf('0\n');
        return;
    end
    w = length(grid_lines{1});

    antennas = containers.Map();
    for y = 0:h-1
        line_str = grid_lines{y+1};
        for x = 0:w-1
            c = line_str(x+1);
            if c ~= '.'
                if isKey(antennas, c)
                    antennas(c) = [antennas(c); y, x];
                else
                    antennas(c) = [y, x];
                end
            end
        end
    end

    lines_per_freq = containers.Map();
    freq_keys = keys(antennas);
    for k_idx = 1:numel(freq_keys)
        f = freq_keys{k_idx};
        coords = antennas(f);
        n = size(coords, 1);
        
        current_freq_lines_list = cell(0,1);
        line_count = 0;
        
        for i = 1:n
            for j = i+1:n
                A = coords(i, :);
                B = coords(j, :);
                
                dy = B(1) - A(1);
                dx = B(2) - A(2);
                
                g = gcd(dy, dx);
                sy = dy / g;
                sx = dx / g;
                
                if sx < 0 || (sx == 0 && sy < 0)
                    sx = -sx;
                    sy = -sy;
                end
                
                c = sy * A(2) - sx * A(1);
                
                line_count = line_count + 1;
                current_freq_lines_list{line_count} = [sx, sy, c];
            end
        end
        
        if line_count > 0
            current_freq_lines_matrix = vertcat(current_freq_lines_list{:});
            lines_per_freq(f) = unique(current_freq_lines_matrix, 'rows');
        else
            lines_per_freq(f) = zeros(0,3);
        end
    end

    all_lines_list = cell(0,1);
    line_freq_keys = keys(lines_per_freq);
    for k_idx = 1:numel(line_freq_keys)
        f = line_freq_keys{k_idx};
        all_lines_list{end+1} = lines_per_freq(f);
    end
    
    if isempty(all_lines_list)
        fprintf('0\n');
        return;
    end
    
    all_lines_matrix = unique(vertcat(all_lines_list{:}), 'rows');

    max_antinodes = h * w;
    antinodes_coords = zeros(max_antinodes, 2);
    current_antinode_count = 0;
    
    for k = 1:size(all_lines_matrix, 1)
        sx = all_lines_matrix(k, 1);
        sy = all_lines_matrix(k, 2);
        c = all_lines_matrix(k, 3);
        
        if sy == 0
            if mod(c, sx) == 0
                y_val = -c / sx;
                if y_val >= 0 && y_val < h && floor(y_val) == y_val
                    y_val = round(y_val);
                    
                    new_antinodes_count = w;
                    antinodes_coords(current_antinode_count + 1 : current_antinode_count + new_antinodes_count, :) = ...
                        [repmat(y_val, w, 1), (0:w-1)'];
                    current_antinode_count = current_antinode_count + new_antinodes_count;
                end
            end
        elseif sx == 0
            if mod(c, sy) == 0
                x_val = c / sy;
                if x_val >= 0 && x_val < w && floor(x_val) == x_val
                    x_val = round(x_val);
                    
                    new_antinodes_count = h;
                    antinodes_coords(current_antinode_count + 1 : current_antinode_count + new_antinodes_count, :) = ...
                        [(0:h-1)', repmat(x_val, h, 1)];
                    current_antinode_count = current_antinode_count + new_antinodes_count;
                end
            end
        else
            y_coords_all = (0:h-1)';
            val = c + sx * y_coords_all;
            
            is_divisible = (mod(val, sy) == 0);
            
            if any(is_divisible)
                x_coords_raw = val(is_divisible) / sy;
                y_coords_filtered = y_coords_all(is_divisible);
                
                valid_x_indices = x_coords_raw >= 0 & x_coords_raw < w & floor(x_coords_raw) == x_coords_raw;
                
                if any(valid_x_indices)
                    new_antinodes_count = sum(valid_x_indices);
                    antinodes_coords(current_antinode_count + 1 : current_antinode_count + new_antinodes_count, :) = ...
                        [y_coords_filtered(valid_x_indices), round(x_coords_raw(valid_x_indices))];
                    current_antinode_count = current_antinode_count + new_antinodes_count;
                end
            end
        end
    end

    final_antinodes = unique(antinodes_coords(1:current_antinode_count, :), 'rows');
    fprintf('%d\n', size(final_antinodes, 1));
end
