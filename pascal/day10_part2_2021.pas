program CompletionScore;
var
  f: Text;
  line: String;
  stack: array[1..2000] of Char;
  sSize, i, len, count: Integer;
  corrupted: Boolean;
  c: Char;
  scr: QWord;
  scores: array of QWord;

function OpeningScore(ch: Char): QWord;
begin
  case ch of
    '(': OpeningScore := 1;
    '[': OpeningScore := 2;
    '{': OpeningScore := 3;
    '<': OpeningScore := 4;
  else
    OpeningScore := 0;
  end;
end;

procedure QuickSort(var a: array of QWord; lo, hi: Integer);
var
  i, j: Integer;
  pivot, tmp: QWord;
begin
  i := lo;
  j := hi;
  pivot := a[(lo + hi) shr 1];
  while i <= j do
  begin
    while a[i] < pivot do Inc(i);
    while a[j] > pivot do Dec(j);
    if i <= j then
    begin
      tmp := a[i];
      a[i] := a[j];
      a[j] := tmp;
      Inc(i);
      Dec(j);
    end;
  end;
  if lo < j then QuickSort(a, lo, j);
  if i < hi then QuickSort(a, i, hi);
end;

begin
  Assign(f, 'input.txt');
  Reset(f);
  count := 0;
  while not Eof(f) do
  begin
    ReadLn(f, line);
    sSize := 0;
    corrupted := False;
    len := Length(line);
    for i := 1 to len do
    begin
      c := line[i];
      if (c = '(') or (c = '[') or (c = '{') or (c = '<') then
      begin
        Inc(sSize);
        stack[sSize] := c;
      end
      else
      begin
        if sSize = 0 then
          corrupted := True
        else
        begin
          case c of
            ')': if stack[sSize] <> '(' then corrupted := True;
            ']': if stack[sSize] <> '[' then corrupted := True;
            '}': if stack[sSize] <> '{' then corrupted := True;
            '>': if stack[sSize] <> '<' then corrupted := True;
          end;
          Dec(sSize);
        end;
        if corrupted then Break;
      end;
    end;
    if (not corrupted) and (sSize > 0) then
    begin
      scr := 0;
      for i := sSize downto 1 do
        scr := scr * 5 + OpeningScore(stack[i]);
      SetLength(scores, count + 1);
      scores[count] := scr;
      Inc(count);
    end;
  end;
  Close(f);
  if count > 0 then
  begin
    QuickSort(scores, 0, count - 1);
    WriteLn(scores[count div 2]);
  end
  else
    WriteLn(0);
end.
