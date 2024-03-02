program Solution;

uses SysUtils;

var
    inputFile: Text;
    lines: array of String;
    i, j, k, diff: Integer;
    common: String;

begin
    Assign(inputFile, 'input.txt');
    Reset(inputFile);

    SetLength(lines, 0);
    while not Eof(inputFile) do
    begin
        SetLength(lines, Length(lines) + 1);
        Readln(inputFile, lines[High(lines)]);
    end;

    Close(inputFile);

    for i := 0 to Length(lines) - 2 do
    begin
        for j := i + 1 to Length(lines) - 1 do
        begin
            diff := 0;
            for k := 1 to Length(lines[i]) do
            begin
                if lines[i][k] <> lines[j][k] then
                begin
                    diff := diff + 1;
                    if diff > 1 then
                        Break;
                end;
            end;
            if diff = 1 then
            begin
                common := '';
                for k := 1 to Length(lines[i]) do
                begin
                    if lines[i][k] = lines[j][k] then
                        common := common + lines[i][k];
                end;
                Writeln(common);
                Halt;
            end;
        end;
    end;
end.