
function main
    banks = fscanf(fopen('input.txt'), '%d');
    history = [];
    
    cycles = 0;
    while true
        idx = find_state(history, banks);
        if idx
            fprintf('%d\n', cycles - idx + 1);
            return
        end
        
        history = [history, banks];
        
        [~, max_idx] = max(banks);
        blocks = banks(max_idx);
        banks(max_idx) = 0;
        
        n = length(banks);
        for k = 1:blocks
            banks(mod(max_idx + k - 1, n) + 1) = banks(mod(max_idx + k - 1, n) + 1) + 1;
        end
        
        cycles = cycles + 1;
    end
end

function idx = find_state(history, state)
    for i = 1:size(history, 2)
        if all(history(:, i) == state)
            idx = i;
            return
        end
    end
    idx = 0;
end
