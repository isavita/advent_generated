program Day4PassportProcessing;

var
    inputFile: Text;
    line: string;
    passportData: string;
    validPassports: integer;
    requiredFields: array[1..7] of string = ('byr:', 'iyr:', 'eyr:', 'hgt:', 'hcl:', 'ecl:', 'pid:');

function isValidPassport(data: string): boolean;
var
    i: integer;
begin
    isValidPassport := true;
    for i := 1 to 7 do
    begin
        if Pos(requiredFields[i], data) = 0 then
        begin
            isValidPassport := false;
            break;
        end;
    end;
end;

begin
    assign(inputFile, 'input.txt');
    reset(inputFile);

    validPassports := 0;
    passportData := '';

    while not eof(inputFile) do
    begin
        readln(inputFile, line);
        if line = '' then
        begin
            if isValidPassport(passportData) then
                validPassports := validPassports + 1;
            passportData := '';
        end
        else
        begin
            passportData := passportData + ' ' + line;
        end;
    end;

    if isValidPassport(passportData) then
        validPassports := validPassports + 1;

    writeln(validPassports);

    close(inputFile);
end.