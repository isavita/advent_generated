program Day18;

var
    input: string;
    numRows, numCols, i, j, count: integer;
    grid: array of array of char;

function isTrap(left, center, right: char): char;
begin
    if (left = '^') and (center = '^') and (right = '.') then
        isTrap := '^'
    else if (center = '^') and (right = '^') and (left = '.') then
        isTrap := '^'
    else if (left = '^') and (center = '.') and (right = '.') then
        isTrap := '^'
    else if (right = '^') and (center = '.') and (left = '.') then
        isTrap := '^'
    else
        isTrap := '.';
end;

begin
    assign(input, 'input.txt');
    reset(input);

    readln(input, input);
    numRows := 40;
    numCols := length(input);
    setLength(grid, numRows, numCols);

    for i := 1 to numCols do
        grid[0][i-1] := input[i];

    for i := 1 to numRows - 1 do
    begin
        for j := 0 to numCols - 1 do
        begin
            if j = 0 then
                grid[i][j] := isTrap('.', grid[i-1][j], grid[i-1][j+1])
            else if j = numCols - 1 then
                grid[i][j] := isTrap(grid[i-1][j-1], grid[i-1][j], '.')
            else
                grid[i][j] := isTrap(grid[i-1][j-1], grid[i-1][j], grid[i-1][j+1]);
        end;
    end;

    count := 0;
    for i := 0 to numRows - 1 do
    begin
        for j := 0 to numCols - 1 do
        begin
            if grid[i][j] = '.' then
                count := count + 1;
        end;
    end;

    writeln(count);

    close(input);
end.