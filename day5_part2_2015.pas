program Day5;

var
  inputFile: TextFile;
  line: string;
  niceCount: integer;
  niceCountPartTwo: integer;

function isNice(s: string): boolean;
var
  i, vowels, doubles: integer;
  hasDisallowed: boolean;
begin
  vowels := 0;
  doubles := 0;
  hasDisallowed := false;

  for i := 1 to Length(s) do
  begin
    case s[i] of
      'a', 'e', 'i', 'o', 'u': Inc(vowels);
    end;

    if (i > 1) and (s[i] = s[i - 1]) then Inc(doubles);

    case s[i - 1] + s[i] of
      'ab', 'cd', 'pq', 'xy': hasDisallowed := true;
    end;
  end;

  isNice := (vowels >= 3) and (doubles >= 1) and (not hasDisallowed);
end;

function isNicePartTwo(s: string): boolean;
var
  i: integer;
begin
  for i := 1 to Length(s) - 2 do
  begin
    if Pos(Copy(s, i, 2), Copy(s, i + 2, Length(s) - i - 1)) > 0 then
    begin
      for i := 3 to Length(s) do
      begin
        if (s[i] = s[i - 2]) then
        begin
          isNicePartTwo := true;
          Exit;
        end;
      end;
    end;
  end;

  isNicePartTwo := false;
end;

begin
  AssignFile(inputFile, 'input.txt');
  Reset(inputFile);

  niceCount := 0;
  niceCountPartTwo := 0;

  while not EOF(inputFile) do
  begin
    ReadLn(inputFile, line);

    if isNice(line) then Inc(niceCount);
    if isNicePartTwo(line) then Inc(niceCountPartTwo);
  end;

  CloseFile(inputFile);

  WriteLn('Nice strings count (Part One): ', niceCount);
  WriteLn('Nice strings count (Part Two): ', niceCountPartTwo);
end.