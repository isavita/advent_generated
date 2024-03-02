program solution;

uses SysUtils;

var
    inputFile: Text;
    id: string;
    charCount: array[Char] of Integer;
    idChar: Char;
    count: Integer;
    twos, threes: Boolean;
    twoCount, threeCount, checksum: Integer;

begin
    Assign(inputFile, 'input.txt');
    Reset(inputFile);

    twoCount := 0;
    threeCount := 0;

    while not Eof(inputFile) do
    begin
        Readln(inputFile, id);
        FillChar(charCount, SizeOf(charCount), 0);
        twos := False;
        threes := False;

        for idChar in id do
            Inc(charCount[idChar]);

        for count in charCount do
        begin
            if count = 2 then
                twos := True
            else if count = 3 then
                threes := True;
        end;

        if twos then
            Inc(twoCount);
        if threes then
            Inc(threeCount);
    end;

    checksum := twoCount * threeCount;
    Writeln(checksum);

    Close(inputFile);
end.