
function main()
    MAX_NUMBERS = 30000000;

    fid = fopen('input.txt', 'r');
    line = fgetl(fid);
    fclose(fid);
    
    start_numbers = str2double(strsplit(line, ','));
    
    spoken = zeros(1, MAX_NUMBERS, 'uint32');
    
    num_starts = numel(start_numbers);
    for i = 1:num_starts-1
        spoken(start_numbers(i) + 1) = i;
    end
    
    lastSpoken = start_numbers(end);
    
    for turn = num_starts : MAX_NUMBERS - 1
        prev_turn = spoken(lastSpoken + 1);
        spoken(lastSpoken + 1) = turn;
        
        if prev_turn == 0
            lastSpoken = 0;
        else
            lastSpoken = turn - prev_turn;
        end
    end
    
    fprintf('%d\n', lastSpoken);
end
