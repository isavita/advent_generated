program solution;

uses sysutils, classes;

var
    inputFile: Text;
    messages: TStringList;
    correctedMessage: string;
    messageLength, i, j: integer;
    count: array of array of integer;
    charCount: array of integer;
    maxChar: char;
    maxCount, cnt: integer;

function getMostCommonChar(count: array of integer): char;
var
    maxChar: char;
    maxCount, i: integer;
begin
    maxCount := 0;
    for i := low(count) to high(count) do
    begin
        if count[i] > maxCount then
        begin
            maxCount := count[i];
            maxChar := chr(i);
        end;
    end;
    getMostCommonChar := maxChar;
end;

begin
    assign(inputFile, 'input.txt');
    reset(inputFile);

    messages := TStringList.Create;
    while not eof(inputFile) do
    begin
        readln(inputFile, correctedMessage);
        messages.Add(correctedMessage);
    end;

    close(inputFile);

    if messages.Count = 0 then
    begin
        writeln('');
        exit;
    end;

    messageLength := length(messages[0]);
    setLength(count, messageLength);
    for i := low(count) to high(count) do
    begin
        setLength(count[i], 256);
    end;

    for i := 0 to messageLength - 1 do
    begin
        for j := 0 to messages.Count - 1 do
        begin
            count[i][ord(messages[j][i + 1])] := count[i][ord(messages[j][i + 1])] + 1;
        end;
    end;

    for i := 0 to messageLength - 1 do
    begin
        maxChar := getMostCommonChar(count[i]);
        write(maxChar);
    end;

    writeln('');
end.