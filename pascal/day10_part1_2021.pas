
program SyntaxScoring;

{$O+,Q+}

type
  TCharStack = array[0..1000] of Char;

var
  inputFile: Text;
  line: string;
  totalScore: LongInt;
  stack: TCharStack;
  stackTop: Integer;

function GetScore(c: Char): LongInt;
begin
  case c of
    ')': GetScore := 3;
    ']': GetScore := 57;
    '}': GetScore := 1197;
    '>': GetScore := 25137;
  else
    GetScore := 0;
  end;
end;

function GetPair(c: Char): Char;
begin
  case c of
    ')': GetPair := '(';
    ']': GetPair := '[';
    '}': GetPair := '{';
    '>': GetPair := '<';
  else
    GetPair := #0;
  end;
end;

function CheckLine(line: string): LongInt;
var
  i: Integer;
  c: Char;
begin
  stackTop := -1;
  CheckLine := 0;
  for i := 1 to Length(line) do
  begin
    c := line[i];
    case c of
      '(', '[', '{', '<':
        begin
          Inc(stackTop);
          stack[stackTop] := c;
        end;
      ')', ']', '}', '>':
        begin
          if (stackTop = -1) or (stack[stackTop] <> GetPair(c)) then
          begin
            CheckLine := GetScore(c);
            Exit;
          end;
          Dec(stackTop);
        end;
    end;
  end;
end;

begin
  Assign(inputFile, 'input.txt');
  Reset(inputFile);
  totalScore := 0;
  while not Eof(inputFile) do
  begin
    ReadLn(inputFile, line);
    totalScore := totalScore + CheckLine(line);
  end;
  Close(inputFile);
  WriteLn(totalScore);
end.
