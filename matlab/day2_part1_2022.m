
function main()
    fid = fopen('input.txt', 'r');
    total_score = 0;
    
    while ~feof(fid)
        line = strtrim(fgetl(fid));
        opponent = line(1);
        your_move = line(3);
        
        score = 0;
        if your_move == 'X'
            score = 1;
        elseif your_move == 'Y'
            score = 2;
        elseif your_move == 'Z'
            score = 3;
        end
        
        if (opponent == 'A' && your_move == 'Y') || ...
           (opponent == 'B' && your_move == 'Z') || ...
           (opponent == 'C' && your_move == 'X')
            score = score + 6;
        elseif (opponent == 'A' && your_move == 'X') || ...
               (opponent == 'B' && your_move == 'Y') || ...
               (opponent == 'C' && your_move == 'Z')
            score = score + 3;
        end
        
        total_score = total_score + score;
    end
    
    fclose(fid);
    disp(total_score);
end

main();
