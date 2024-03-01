program Day3;

var
    inputFile: Text;
    a, b, c, countPossibleTriangles: Integer;

begin
    assign(inputFile, 'input.txt');
    reset(inputFile);

    countPossibleTriangles := 0;

    while not eof(inputFile) do
    begin
        readln(inputFile, a, b, c);

        if (a + b > c) and (a + c > b) and (b + c > a) then
            countPossibleTriangles := countPossibleTriangles + 1;
    end;

    close(inputFile);

    writeln(countPossibleTriangles);
end.