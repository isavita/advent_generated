program solution;

uses sysutils;

function itemPriority(item: char): integer;
begin
    if (item >= 'a') and (item <= 'z') then
        itemPriority := ord(item) - ord('a') + 1
    else
        itemPriority := ord(item) - ord('A') + 27;
end;

var
    fileInput: Text;
    line, firstCompartment, secondCompartment: string;
    sum: integer;
    item: char;
    compartmentMap: array[Char] of integer;
    half, i: integer;

begin
    Assign(fileInput, 'input.txt');
    Reset(fileInput);

    sum := 0;

    while not eof(fileInput) do
    begin
        Readln(fileInput, line);
        half := Length(line) div 2;
        firstCompartment := Copy(line, 1, half);
        secondCompartment := Copy(line, half + 1, Length(line));

        FillChar(compartmentMap, SizeOf(compartmentMap), 0);

        for i := 1 to Length(firstCompartment) do
        begin
            item := firstCompartment[i];
            compartmentMap[item] := compartmentMap[item] + 1;
        end;

        for i := 1 to Length(secondCompartment) do
        begin
            item := secondCompartment[i];
            if compartmentMap[item] > 0 then
            begin
                sum := sum + itemPriority(item);
                break;
            end;
        end;
    end;

    Close(fileInput);

    writeln(sum);
end.