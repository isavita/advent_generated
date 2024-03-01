program Day1NotQuiteLisp;

var
    inputFile: text;
    currentFloor, currentChar: char;
    floorNum, charNum: integer;

begin
    assign(inputFile, 'input.txt');
    reset(inputFile);

    currentFloor := '0';
    floorNum := 0;
    charNum := 0;

    while not eof(inputFile) do
    begin
        read(inputFile, currentChar);
        charNum := charNum + 1;

        if currentChar = '(' then
            floorNum := floorNum + 1
        else if currentChar = ')' then
            floorNum := floorNum - 1;
        
        if floorNum = -1 then
        begin
            writeln('Santa entered the basement at character: ', charNum);
        end;
    end;

    writeln('Santa ended up on floor: ', floorNum);

    close(inputFile);
end.