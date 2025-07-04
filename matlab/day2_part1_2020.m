
function main()
    valid_passwords = 0;
    file_id = fopen('input.txt', 'r');
    
    while ~feof(file_id)
        line = strtrim(fgets(file_id));
        
        if isempty(line)
            continue;
        end
        
        parts = strsplit(line, ': ');
        policy = parts{1};
        password = parts{2};
        
        policy_parts = strsplit(policy, ' ');
        limits = policy_parts{1};
        letter = policy_parts{2};
        
        count_limits = strsplit(limits, '-');
        min_count = str2double(count_limits{1});
        max_count = str2double(count_limits{2});
        
        count = sum(password == letter);
        
        if count >= min_count && count <= max_count
            valid_passwords = valid_passwords + 1;
        end
    end
    
    fclose(file_id);
    disp(valid_passwords);
end

main();
