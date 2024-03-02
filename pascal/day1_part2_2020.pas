program solution;

var
    inputFile: text;
    expenses: array of integer;
    i, j, k, num: integer;

begin
    assign(inputFile, 'input.txt');
    reset(inputFile);

    i := 0;
    while not eof(inputFile) do
    begin
        readln(inputFile, num);
        SetLength(expenses, i+1);
        expenses[i] := num;
        i := i + 1;
    end;

    close(inputFile);

    for i := 0 to length(expenses) - 1 do
    begin
        for j := i + 1 to length(expenses) - 1 do
        begin
            for k := j + 1 to length(expenses) - 1 do
            begin
                if expenses[i] + expenses[j] + expenses[k] = 2020 then
                begin
                    writeln(expenses[i] * expenses[j] * expenses[k]);
                    halt;
                end;
            end;
        end;
    end;
end.