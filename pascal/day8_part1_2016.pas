
program ScreenSimulator;

const
  ScreenHeight = 6;
  ScreenWidth = 50;

type
  TScreen = array[0..ScreenHeight - 1, 0..ScreenWidth - 1] of Byte;
  TRow = array[0..ScreenWidth - 1] of Byte;
  TColumn = array[0..ScreenHeight - 1] of Byte;

var
  screen: TScreen;
  inputFile: Text;
  line: string;
  i, j, p1, p2, code: Integer;
  s1, s2: string;
  a, b, x, y: Integer;
  litPixels: Integer;

procedure Rect(var scr: TScreen; width, height: Integer);
var
  r, c: Integer;
begin
  for r := 0 to height - 1 do
    for c := 0 to width - 1 do
      if (r < ScreenHeight) and (c < ScreenWidth) then
        scr[r, c] := 1;
end;

procedure RotateRow(var scr: TScreen; rowIdx, amount: Integer);
var
  tempRow: TRow;
  c, newIndex: Integer;
begin
  if (rowIdx < 0) or (rowIdx >= ScreenHeight) then Exit;

  amount := amount mod ScreenWidth;
  if amount = 0 then Exit;

  for c := 0 to ScreenWidth - 1 do
    tempRow[c] := scr[rowIdx, c];

  for c := 0 to ScreenWidth - 1 do
  begin
    newIndex := (c + amount) mod ScreenWidth;
    scr[rowIdx, newIndex] := tempRow[c];
  end;
end;

procedure RotateColumn(var scr: TScreen; colIdx, amount: Integer);
var
  tempCol: TColumn;
  r, newIndex: Integer;
begin
  if (colIdx < 0) or (colIdx >= ScreenWidth) then Exit;

  amount := amount mod ScreenHeight;
   if amount = 0 then Exit;

  for r := 0 to ScreenHeight - 1 do
    tempCol[r] := scr[r, colIdx];

  for r := 0 to ScreenHeight - 1 do
  begin
    newIndex := (r + amount) mod ScreenHeight;
    scr[newIndex, colIdx] := tempCol[r];
  end;
end;

begin
  for i := 0 to ScreenHeight - 1 do
    for j := 0 to ScreenWidth - 1 do
      screen[i, j] := 0;

  Assign(inputFile, 'input.txt');
  Reset(inputFile);

  while not Eof(inputFile) do
  begin
    ReadLn(inputFile, line);

    if Pos('rect ', line) = 1 then
    begin
      p1 := Pos(' ', line);
      p2 := Pos('x', line);
      s1 := Copy(line, p1 + 1, p2 - p1 - 1);
      s2 := Copy(line, p2 + 1, Length(line) - p2);
      Val(s1, a, code);
      Val(s2, b, code);
      Rect(screen, a, b);
    end
    else if Pos('rotate row y=', line) > 0 then
    begin
      p1 := Pos('=', line);
      p2 := Pos(' by ', line);
      s1 := Copy(line, p1 + 1, p2 - p1 - 1);
      s2 := Copy(line, p2 + 4, Length(line) - p2 - 3);
      Val(s1, y, code);
      Val(s2, b, code);
      RotateRow(screen, y, b);
    end
    else if Pos('rotate column x=', line) > 0 then
    begin
      p1 := Pos('=', line);
      p2 := Pos(' by ', line);
      s1 := Copy(line, p1 + 1, p2 - p1 - 1);
      s2 := Copy(line, p2 + 4, Length(line) - p2 - 3);
      Val(s1, x, code);
      Val(s2, b, code);
      RotateColumn(screen, x, b);
    end;
  end;

  Close(inputFile);

  litPixels := 0;
  for i := 0 to ScreenHeight - 1 do
    for j := 0 to ScreenWidth - 1 do
      litPixels := litPixels + screen[i, j];

  WriteLn(litPixels);
end.
