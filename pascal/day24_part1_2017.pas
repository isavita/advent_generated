program Solution;

var
    maxStrength: integer;

type
    Component = record
        a, b: integer;
    end;

procedure findStrongestBridge(components: array of Component; var used: array of boolean; port, strength: integer);
var
    i, nextPort: integer;
    c: Component;
begin
    if strength > maxStrength then
        maxStrength := strength;

    for i := 0 to High(components) do
    begin
        if used[i] then
            continue;

        c := components[i];
        if (c.a = port) or (c.b = port) then
        begin
            used[i] := true;
            if c.a = port then
                nextPort := c.b
            else
                nextPort := c.a;
            findStrongestBridge(components, used, nextPort, strength + c.a + c.b);
            used[i] := false;
        end;
    end;
end;

var
    fileText: Text;
    components: array of Component;
    used: array of boolean;
    line, portStr: string;
    a, b, i: integer;

begin
    maxStrength := 0;

    Assign(fileText, 'input.txt');
    Reset(fileText);

    while not Eof(fileText) do
    begin
        ReadLn(fileText, line);
        portStr := Copy(line, 1, Pos('/', line) - 1);
        a := StrToInt(portStr);
        Delete(line, 1, Pos('/', line));
        b := StrToInt(line);
        SetLength(components, Length(components) + 1);
        components[High(components)].a := a;
        components[High(components)].b := b;
    end;

    Close(fileText);

    SetLength(used, Length(components));
    findStrongestBridge(components, used, 0, 0);

    WriteLn(maxStrength);
end.