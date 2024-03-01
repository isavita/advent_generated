program Day5;

var
  inputFile: text;
  line: string;
  niceCount: integer;
  i: integer;

function hasThreeVowels(s: string): boolean;
var
  count: integer;
  i: integer;
begin
  count := 0;
  for i := 1 to Length(s) do
  begin
    if s[i] in ['a', 'e', 'i', 'o', 'u'] then
      Inc(count);
  end;
  hasThreeVowels := count >= 3;
end;

function hasDoubleLetter(s: string): boolean;
var
  i: integer;
begin
  for i := 1 to Length(s) - 1 do
  begin
    if s[i] = s[i+1] then
    begin
      hasDoubleLetter := true;
      Exit;
    end;
  end;
  hasDoubleLetter := false;
end;

function isNice(s: string): boolean;
begin
  if not hasThreeVowels(s) then
    isNice := false
  else if not hasDoubleLetter(s) then
    isNice := false
  else if Pos('ab', s) > 0 then
    isNice := false
  else if Pos('cd', s) > 0 then
    isNice := false
  else if Pos('pq', s) > 0 then
    isNice := false
  else if Pos('xy', s) > 0 then
    isNice := false
  else
    isNice := true;
end;

begin
  Assign(inputFile, 'input.txt');
  Reset(inputFile);

  niceCount := 0;

  while not Eof(inputFile) do
  begin
    Readln(inputFile, line);
    if isNice(line) then
      Inc(niceCount);
  end;

  Close(inputFile);

  Writeln(niceCount);
end.