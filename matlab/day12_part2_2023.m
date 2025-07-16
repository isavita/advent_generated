
function main()
    fid = fopen('input.txt', 'r');
    data = textscan(fid, '%s %s');
    fclose(fid);
    
    springs_data = data{1};
    groups_data = data{2};
    
    total_arrangements = 0;
    
    for i = 1:length(springs_data)
        springs = springs_data{i};
        groups = str2double(strsplit(groups_data{i}, ','));
        
        unfolded_springs = strjoin(repmat({springs}, 1, 5), '?');
        unfolded_groups = repmat(groups, 1, 5);
        
        total_arrangements = total_arrangements + solve_row(unfolded_springs, unfolded_groups);
    end
    
    fprintf('%.0f\n', total_arrangements);
end

function count = solve_row(springs, groups)
    len_s = length(springs);
    len_g = length(groups);
    
    max_group_size = 0;
    if ~isempty(groups)
        max_group_size = max(groups);
    end
    
    cache = ones(len_s + 2, len_g + 2, max_group_size + 1) * -1;
    
    count = count_recursive(1, 1, 0);

    function res = count_recursive(i_s, i_g, damaged_len)
        if cache(i_s, i_g, damaged_len + 1) ~= -1
            res = cache(i_s, i_g, damaged_len + 1);
            return;
        end
        
        if i_s > len_s
            if i_g > len_g && damaged_len == 0
                res = 1;
            elseif i_g == len_g && damaged_len == groups(i_g)
                res = 1;
            else
                res = 0;
            end
            return;
        end
        
        res = 0;
        char = springs(i_s);
        
        if char == '.' || char == '?'
            if damaged_len == 0
                res = res + count_recursive(i_s + 1, i_g, 0);
            elseif i_g <= len_g && damaged_len == groups(i_g)
                res = res + count_recursive(i_s + 1, i_g + 1, 0);
            end
        end
        
        if char == '#' || char == '?'
            if i_g <= len_g && damaged_len < groups(i_g)
                res = res + count_recursive(i_s + 1, i_g, damaged_len + 1);
            end
        end
        
        cache(i_s, i_g, damaged_len + 1) = res;
    end
end
