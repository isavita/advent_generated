program Solution;

var
    inputFile: text;
    score, depth: integer;
    inGarbage, cancelNext: boolean;
    ch: char;

begin
    // Step 1: Read Input
    assign(inputFile, 'input.txt');
    reset(inputFile);

    // Step 2: Initialize Variables
    score := 0;
    depth := 0;
    inGarbage := false;
    cancelNext := false;

    // Step 3: Process Stream
    while not eof(inputFile) do
    begin
        read(inputFile, ch);
        if cancelNext then
        begin
            cancelNext := false;
            continue;
        end;

        if inGarbage then
        begin
            if ch = '!' then
            begin
                cancelNext := true;
            end
            else if ch = '>' then
            begin
                inGarbage := false;
            end;
        end
        else
        begin
            case ch of
                '{': depth := depth + 1;
                '}':
                begin
                    score := score + depth;
                    depth := depth - 1;
                end;
                '<': inGarbage := true;
            end;
        end;
    end;

    // Step 4: Print Score
    writeln(score);
end.