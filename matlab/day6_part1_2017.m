
function main()
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Error opening file');
    end
    
    line = fgetl(fid);
    fclose(fid);
    
    banks = sscanf(line, '%d');
    bank_count = length(banks);
    
    seen_states = containers.Map('KeyType', 'char', 'ValueType', 'double');
    cycles = 0;
    
    current_state = banks;
    
    while true
        state_key = sprintf('%d,', current_state);
        if isKey(seen_states, state_key)
            break;
        end
        seen_states(state_key) = cycles;
        
        [~, max_index] = max(current_state);
        
        blocks = current_state(max_index);
        current_state(max_index) = 0;
        
        for i = 1:blocks
            current_state(mod(max_index + i - 1, bank_count) + 1) = current_state(mod(max_index + i - 1, bank_count) + 1) + 1;
        end
        
        cycles = cycles + 1;
    end
    
    fprintf('It takes %d redistribution cycles to reach a repeated configuration.\n', cycles);
end

main();
