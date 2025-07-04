
function main()
    fid = fopen('input.txt', 'r');
    lines = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    lines = lines{1};

    total_score = 0;
    for i = 1:length(lines)
        opponent = lines{i}(1);
        round_end = lines{i}(3);

        your_move = ' ';
        if round_end == 'X'
            if opponent == 'A'
                your_move = 'Z';
            elseif opponent == 'B'
                your_move = 'X';
            else
                your_move = 'Y';
            end
        elseif round_end == 'Y'
            if opponent == 'A'
                your_move = 'X';
            elseif opponent == 'B'
                your_move = 'Y';
            else
                your_move = 'Z';
            end
        else
            if opponent == 'A'
                your_move = 'Y';
            elseif opponent == 'B'
                your_move = 'Z';
            else
                your_move = 'X';
            end
        end

        score = 0;
        if your_move == 'X'
            score = 1;
        elseif your_move == 'Y'
            score = 2;
        elseif your_move == 'Z'
            score = 3;
        end

        if (opponent == 'A' && your_move == 'Y') || (opponent == 'B' && your_move == 'Z') || (opponent == 'C' && your_move == 'X')
            score = score + 6;
        elseif (opponent == 'A' && your_move == 'X') || (opponent == 'B' && your_move == 'Y') || (opponent == 'C' && your_move == 'Z')
            score = score + 3;
        end

        total_score = total_score + score;
    end

    disp(total_score);
end
