program Solution;

var
  inputFile: text;
  line: string;
  originalLength, encodedLength, totalDiff: integer;

function calculateEncodedLength(s: string): integer;
var
  encoded: string;
  ch: char;
begin
  encoded := '"';
  for ch in s do
  begin
    if (ch = '\') or (ch = '"') then
    begin
      encoded := encoded + '\';
    end;
    encoded := encoded + ch;
  end;
  encoded := encoded + '"';
  calculateEncodedLength := Length(encoded);
end;

begin
  totalDiff := 0;
  Assign(inputFile, 'input.txt');
  Reset(inputFile);

  while not Eof(inputFile) do
  begin
    Readln(inputFile, line);
    originalLength := Length(line);
    encodedLength := calculateEncodedLength(line);
    totalDiff := totalDiff + (encodedLength - originalLength);
  end;

  Close(inputFile);
  writeln(totalDiff);
end.