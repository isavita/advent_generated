program Day1SonarSweep;

var
    inputFile: text;
    currentDepth, previousDepth, increaseCount: integer;

begin
    assign(inputFile, 'input.txt');
    reset(inputFile);

    readln(inputFile, previousDepth);
    increaseCount := 0;

    while not eof(inputFile) do
    begin
        readln(inputFile, currentDepth);
        if currentDepth > previousDepth then
            increaseCount := increaseCount + 1;
        previousDepth := currentDepth;
    end;

    writeln('Number of measurements larger than the previous measurement: ', increaseCount);

    close(inputFile);
end.