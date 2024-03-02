program Main;

uses SysUtils;

var
    inputFile: Text;
    line: string;
    opponent, roundEnd, yourMove: Char;
    totalScore, score: Integer;

begin
    Assign(inputFile, 'input.txt');
    Reset(inputFile);

    totalScore := 0;

    while not Eof(inputFile) do
    begin
        Readln(inputFile, line);
        opponent := line[1];
        roundEnd := line[3];

        if roundEnd = 'X' then
        begin
            if opponent = 'A' then
                yourMove := 'Z'
            else if opponent = 'B' then
                yourMove := 'X'
            else
                yourMove := 'Y'
        end
        else if roundEnd = 'Y' then
        begin
            if opponent = 'A' then
                yourMove := 'X'
            else if opponent = 'B' then
                yourMove := 'Y'
            else
                yourMove := 'Z'
        end
        else
        begin
            if opponent = 'A' then
                yourMove := 'Y'
            else if opponent = 'B' then
                yourMove := 'Z'
            else
                yourMove := 'X'
        end;

        score := 0;
        if yourMove = 'X' then
            score := 1
        else if yourMove = 'Y' then
            score := 2
        else if yourMove = 'Z' then
            score := 3;

        if (opponent = 'A') and (yourMove = 'Y') or (opponent = 'B') and (yourMove = 'Z') or (opponent = 'C') and (yourMove = 'X') then
            score := score + 6
        else if (opponent = 'A') and (yourMove = 'X') or (opponent = 'B') and (yourMove = 'Y') or (opponent = 'C') and (yourMove = 'Z') then
            score := score + 3;

        totalScore := totalScore + score;
    end;

    Close(inputFile);
    writeln(totalScore);
end.