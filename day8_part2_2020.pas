program solution;

uses sysutils, classes;

var
    f: textfile;
    instructions: TStringList;
    i, accumulator, arg, currentInstruction: integer;
    op: string;
    visited: array of boolean;
    modifiedInstructions: TStringList;
    terminated: boolean;

function parseInstruction(instruction: string): TStringList;
var
    parts: TStringList;
begin
    parts := TStringList.Create;
    parts.DelimitedText := instruction;
    parseInstruction := parts;
end;

function executeBootCode(instructions: TStringList): integer;
var
    accumulator, arg, currentInstruction: integer;
    op: string;
    visited: array of boolean;
begin
    accumulator := 0;
    SetLength(visited, instructions.Count);
    currentInstruction := 0;

    while (currentInstruction < instructions.Count) and (not visited[currentInstruction]) do
    begin
        visited[currentInstruction] := true;
        op := parseInstruction(instructions[currentInstruction])[0];
        arg := StrToInt(parseInstruction(instructions[currentInstruction])[1]);

        case op of
            'acc': begin
                accumulator := accumulator + arg;
                currentInstruction := currentInstruction + 1;
            end;
            'jmp': begin
                currentInstruction := currentInstruction + arg;
            end;
            'nop': begin
                currentInstruction := currentInstruction + 1;
            end;
        end;
    end;

    executeBootCode := accumulator;
end;

begin
    assign(f, 'input.txt');
    reset(f);

    instructions := TStringList.Create;
    while not eof(f) do
    begin
        readln(f, op);
        instructions.Add(op);
    end;

    for i := 0 to instructions.Count - 1 do
    begin
        op := parseInstruction(instructions[i])[0];
        arg := StrToInt(parseInstruction(instructions[i])[1]);

        if op = 'acc' then
            continue;

        modifiedInstructions := TStringList.Create;
        modifiedInstructions.Assign(instructions);
        if op = 'jmp' then
            modifiedInstructions[i] := 'nop ' + IntToStr(arg)
        else
            modifiedInstructions[i] := 'jmp ' + IntToStr(arg);

        accumulator := executeBootCode(modifiedInstructions);
        writeln(accumulator);
    end;

    close(f);
end.