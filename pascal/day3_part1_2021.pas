program solution;

var
    fileInput: Text;
    counts: array[1..12, 0..1] of integer;
    i, gammaRate, epsilonRate: integer;
    num: string;

begin
    assign(fileInput, 'input.txt');
    reset(fileInput);

    for i := 1 to 12 do
    begin
        counts[i][0] := 0;
        counts[i][1] := 0;
    end;

    while not eof(fileInput) do
    begin
        readln(fileInput, num);
        for i := 1 to length(num) do
        begin
            counts[i][ord(num[i]) - ord('0')] := counts[i][ord(num[i]) - ord('0')] + 1;
        end;
    end;

    gammaRate := 0;
    epsilonRate := 0;
    for i := 1 to 12 do
    begin
        if counts[i][0] > counts[i][1] then
            gammaRate := gammaRate or (1 shl (12 - i))
        else
            epsilonRate := epsilonRate or (1 shl (12 - i));
    end;

    writeln(gammaRate * epsilonRate);

    close(fileInput);
end.