program Solution;

var
    x: array of Integer;
    line: String;
    n, i, sum: Integer;
    inputFile: Text;

begin
    Assign(inputFile, 'input.txt');
    Reset(inputFile);

    SetLength(x, 1);
    x[0] := 1;

    while not Eof(inputFile) do
    begin
        Readln(inputFile, line);
        case line of
            'noop': begin
                SetLength(x, Length(x) + 1);
                x[Length(x) - 1] := x[Length(x) - 2];
            end;
            else begin
                Delete(line, 1, 5);
                Val(line, n);
                SetLength(x, Length(x) + 2);
                x[Length(x) - 2] := x[Length(x) - 3];
                x[Length(x) - 1] := x[Length(x) - 2] + n;
            end;
        end;
    end;

    sum := 0;
    for i := 0 to Length(x) - 1 do
    begin
        if (i - 19) mod 40 = 0 then
            sum := sum + (i + 1) * x[i];
    end;

    Close(inputFile);
    Writeln(sum);
end.