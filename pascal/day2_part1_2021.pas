program Dive;

var
    inputFile: text;
    command: string;
    action: string;
    value: integer;
    horizontalPosition: integer = 0;
    depth: integer = 0;

begin
    assign(inputFile, 'input.txt');
    reset(inputFile);

    while not eof(inputFile) do
    begin
        readln(inputFile, command);
        action := copy(command, 1, pos(' ', command) - 1);
        val(copy(command, pos(' ', command) + 1, length(command)), value);

        if action = 'forward' then
            horizontalPosition := horizontalPosition + value
        else if action = 'down' then
            depth := depth + value
        else if action = 'up' then
            depth := depth - value;
    end;

    writeln('Final horizontal position: ', horizontalPosition);
    writeln('Final depth: ', depth);
    writeln('Multiplication result: ', horizontalPosition * depth);

    close(inputFile);
end.