program NextPassword;

var
    currentPassword: string;

function readInput(filename: string): string;
var
    f: text;
    line: string;
begin
    assign(f, filename);
    reset(f);
    readln(f, line);
    close(f);
    readInput := line;
end;

function incrementPassword(password: string): string;
var
    i: integer;
begin
    for i := length(password) downto 1 do
    begin
        if password[i] = 'z' then
            password[i] := 'a'
        else
        begin
            password[i] := chr(ord(password[i]) + 1);
            break;
        end;
    end;
    incrementPassword := password;
end;

function hasStraight(password: string): boolean;
var
    i: integer;
begin
    hasStraight := false;
    for i := 1 to length(password) - 2 do
        if (ord(password[i]) + 1 = ord(password[i + 1])) and (ord(password[i]) + 2 = ord(password[i + 2])) then
            hasStraight := true;
end;

function containsInvalidLetters(password: string): boolean;
var
    i: integer;
begin
    containsInvalidLetters := false;
    for i := 1 to length(password) do
        if (password[i] = 'i') or (password[i] = 'o') or (password[i] = 'l') then
            containsInvalidLetters := true;
end;

function hasTwoPairs(password: string): boolean;
var
    count, i: integer;
begin
    count := 0;
    i := 1;
    while i < length(password) do
    begin
        if password[i] = password[i + 1] then
        begin
            count := count + 1;
            i := i + 2; // Skip the next character
        end
        else
            i := i + 1;
    end;
    hasTwoPairs := count >= 2;
end;

function isValidPassword(password: string): boolean;
begin
    isValidPassword := hasStraight(password) and not containsInvalidLetters(password) and hasTwoPairs(password);
end;

function findNextPassword(password: string): string;
begin
    repeat
        password := incrementPassword(password);
    until isValidPassword(password);
    findNextPassword := password;
end;

begin
    currentPassword := readInput('input.txt');
    writeln(findNextPassword(currentPassword));
end.