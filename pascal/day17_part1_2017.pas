program Solution;

var
    inputFile: text;
    steps, currentPos, i: integer;
    buffer: array of integer;

begin
    assign(inputFile, 'input.txt');
    reset(inputFile);
    readln(inputFile, steps);
    close(inputFile);

    SetLength(buffer, 1);
    buffer[0] := 0;
    currentPos := 0;

    for i := 1 to 2017 do
    begin
        currentPos := (currentPos + steps) mod Length(buffer);
        SetLength(buffer, Length(buffer) + 1);
        Move(buffer[currentPos], buffer[currentPos+1], (Length(buffer) - currentPos - 1) * SizeOf(Integer));
        buffer[currentPos+1] := i;
        currentPos := currentPos + 1;
    end;

    for i := 0 to Length(buffer) - 1 do
    begin
        if buffer[i] = 2017 then
        begin
            writeln(buffer[(i+1) mod Length(buffer)]);
            break;
        end;
    end;
end.