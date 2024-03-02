program Solution;

var
    f: text;
    ch: char;
    score, depth, garbageCount: integer;
    inGarbage, cancelNext: boolean;

begin
    assign(f, 'input.txt');
    reset(f);

    score := 0;
    depth := 0;
    inGarbage := false;
    cancelNext := false;
    garbageCount := 0;

    while not eof(f) do
    begin
        read(f, ch);
        if cancelNext then
        begin
            cancelNext := false;
            continue;
        end;

        if inGarbage then
        begin
            if ch = '!' then
                cancelNext := true
            else if ch = '>' then
                inGarbage := false
            else
                garbageCount := garbageCount + 1;
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

    close(f);

    writeln(garbageCount);
end.