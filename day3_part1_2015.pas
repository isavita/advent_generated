program Day3;

var
    input: text;
    currentChar: char;
    x, y: integer;
    houses: array of array of boolean;
    count: integer;

begin
    assign(input, 'input.txt');
    reset(input);

    x := 500;
    y := 500;
    count := 1;

    SetLength(houses, 1000, 1000);
    houses[x, y] := true;

    while not eof(input) do
    begin
        read(input, currentChar);

        case currentChar of
            '^': y := y + 1;
            'v': y := y - 1;
            '>': x := x + 1;
            '<': x := x - 1;
        end;

        if not houses[x, y] then
        begin
            houses[x, y] := true;
            count := count + 1;
        end;
    end;

    close(input);

    writeln(count);
end.