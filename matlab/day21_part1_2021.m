
function main()
    fid = fopen('input.txt', 'r');
    line1 = fgetl(fid);
    line2 = fgetl(fid);
    fclose(fid);

    player1Start = str2double(extractAfter(line1, 'starting position: '));
    player2Start = str2double(extractAfter(line2, 'starting position: '));

    player1Pos = player1Start;
    player2Pos = player2Start;
    player1Score = 0;
    player2Score = 0;
    dieRoll = 1;
    rollCount = 0;

    while true
        rolls = mod(dieRoll - 1, 100) + 1 + mod(dieRoll, 100) + 1 + mod(dieRoll + 1, 100) + 1;
        rollCount = rollCount + 3;
        dieRoll = dieRoll + 3;

        player1Pos = mod(player1Pos + rolls - 1, 10) + 1;
        player1Score = player1Score + player1Pos;

        if player1Score >= 1000
            fprintf('%d\n', player2Score * rollCount);
            break;
        end

        rolls = mod(dieRoll - 1, 100) + 1 + mod(dieRoll, 100) + 1 + mod(dieRoll + 1, 100) + 1;
        rollCount = rollCount + 3;
        dieRoll = dieRoll + 3;

        player2Pos = mod(player2Pos + rolls - 1, 10) + 1;
        player2Score = player2Score + player2Pos;

        if player2Score >= 1000
            fprintf('%d\n', player1Score * rollCount);
            break;
        end
    end
end

function result = extractAfter(str, delimiter)
    idx = strfind(str, delimiter);
    if isempty(idx)
        result = '';
    else
        result = str(idx + length(delimiter):end);
    end
end
