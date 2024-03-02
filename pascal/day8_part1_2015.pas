program Day8Matchsticks;

var
    inputFile: Text;
    line: string;
    codeCount, memoryCount: integer;

function CountCode(line: string): integer;
begin
    CountCode := Length(line);
end;

function CountMemory(line: string): integer;
var
    i, count: integer;
begin
    count := 0;
    i := 2;
    while i <= Length(line) - 1 do
    begin
        if line[i] = '\' then
        begin
            if line[i + 1] = '\' then
            begin
                count := count + 1;
                i := i + 2;
            end
            else if line[i + 1] = '"' then
            begin
                count := count + 1;
                i := i + 2;
            end
            else if line[i + 1] = 'x' then
            begin
                count := count + 1;
                i := i + 4;
            end;
        end
        else
        begin
            count := count + 1;
            i := i + 1;
        end;
    end;
    CountMemory := count;
end;

begin
    Assign(inputFile, 'input.txt');
    Reset(inputFile);

    codeCount := 0;
    memoryCount := 0;

    while not eof(inputFile) do
    begin
        ReadLn(inputFile, line);
        codeCount := codeCount + CountCode(line);
        memoryCount := memoryCount + CountMemory(line);
    end;

    Close(inputFile);

    Writeln(codeCount - memoryCount);
end.