program solution;

uses SysUtils, Classes;

var
    input: Text;
    grid: array of array of char;
    x, y, dx, dy, steps, i: Integer;
    line: String;
begin
    Assign(input, 'input.txt');
    Reset(input);

    SetLength(grid, 0);

    while not Eof(input) do
    begin
        SetLength(grid, Length(grid) + 1);
        Readln(input, line);
        SetLength(grid[Length(grid) - 1], Length(line));

        for i := 1 to Length(line) do
        begin
            grid[Length(grid) - 1][i - 1] := line[i];
        end;
    end;

    Close(input);

    x := 0;
    y := 0;

    for i := Low(grid[0]) to High(grid[0]) do
    begin
        if grid[0][i] = '|' then
        begin
            x := i;
            Break;
        end;
    end;

    dx := 0;
    dy := 1;
    steps := 0;

    repeat
        if (x < 0) or (x >= Length(grid[0])) or (y < 0) or (y >= Length(grid)) then
        begin
            Break;
        end;

        if grid[y][x] = ' ' then
        begin
            Break;
        end;

        Inc(steps);

        if grid[y][x] = '+' then
        begin
            if dx = 0 then
            begin
                if (x > 0) and ((grid[y][x - 1] = '-') or ((grid[y][x - 1] >= 'A') and (grid[y][x - 1] <= 'Z'))) then
                begin
                    dx := -1;
                    dy := 0;
                end
                else
                begin
                    dx := 1;
                    dy := 0;
                end;
            end
            else
            begin
                if (y > 0) and ((grid[y - 1][x] = '|') or ((grid[y - 1][x] >= 'A') and (grid[y - 1][x] <= 'Z'))) then
                begin
                    dx := 0;
                    dy := -1;
                end
                else
                begin
                    dx := 0;
                    dy := 1;
                end;
            end;
        end;

        Inc(x, dx);
        Inc(y, dy);
    until False;

    Writeln(steps);
end.