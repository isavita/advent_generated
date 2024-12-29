
program solve;

{$O+,Q+}

type
  TCharCount = array[char] of integer;
  TCharCounts = array of TCharCount;

var
  messages: array of string;
  messageLength, i, j, minCount, cnt: integer;
  originalMessage: string;
  charCounts: TCharCounts;
  minChar: char;
  f: Text;
  line: string;

function getLeastCommonChar(charCount: TCharCount): char;
var
  minChar: char;
  minCount, c: integer;
begin
  minCount := MaxInt;
  for c := 0 to 255 do
    if charCount[char(c)] > 0 then
      if charCount[char(c)] < minCount then
      begin
        minCount := charCount[char(c)];
        minChar := char(c);
      end;
  getLeastCommonChar := minChar;
end;

begin
  Assign(f, 'input.txt');
  Reset(f);
  SetLength(messages, 0);
  while not Eof(f) do
  begin
    ReadLn(f, line);
    SetLength(messages, Length(messages) + 1);
    messages[Length(messages) - 1] := line;
  end;
  Close(f);

  if Length(messages) = 0 then
  begin
    Writeln('');
    Halt;
  end;

  messageLength := Length(messages[0]);
  SetLength(charCounts, messageLength);
  for i := 0 to messageLength - 1 do
    for j := 0 to 255 do
      charCounts[i][char(j)] := 0;

  for i := 0 to Length(messages) - 1 do
    for j := 1 to Length(messages[i]) do
      charCounts[j - 1][messages[i][j]] := charCounts[j - 1][messages[i][j]] + 1;

  originalMessage := '';
  for i := 0 to messageLength - 1 do
    originalMessage := originalMessage + getLeastCommonChar(charCounts[i]);

  Writeln(originalMessage);
end.
