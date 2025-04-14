
program ScreenSimulator;

const
  ScreenWidth = 50;
  ScreenHeight = 6;

type
  TScreen = array[0..ScreenHeight - 1, 0..ScreenWidth - 1] of Char;

var
  screen: TScreen;
  inputFile: Text;
  instruction: string;
  i, j: Integer;

procedure InitializeScreen(var screen: TScreen);
var
  r, c: Integer;
begin
  for r := 0 to ScreenHeight - 1 do
    for c := 0 to ScreenWidth - 1 do
      screen[r, c] := '.';
end;

procedure Rect(var screen: TScreen; width, height: Integer);
var
  r, c: Integer;
begin
  if (width > ScreenWidth) or (height > ScreenHeight) then Exit; // Basic bounds check

  for r := 0 to height - 1 do
    for c := 0 to width - 1 do
      screen[r, c] := '#';
end;

procedure RotateRow(var screen: TScreen; row, by: Integer);
var
  tempRow: array[0..ScreenWidth - 1] of Char;
  c, sourceCol: Integer;
begin
  if (row < 0) or (row >= ScreenHeight) or (by <= 0) then Exit;

  by := by mod ScreenWidth; // Normalize shift amount
  if by = 0 then Exit;

  // Copy original row to tempRow
  for c := 0 to ScreenWidth - 1 do
    tempRow[c] := screen[row, c];

  // Copy back shifted
  for c := 0 to ScreenWidth - 1 do
  begin
    sourceCol := (c - by + ScreenWidth) mod ScreenWidth;
    screen[row, c] := tempRow[sourceCol];
  end;
end;

procedure RotateColumn(var screen: TScreen; col, by: Integer);
var
  tempCol: array[0..ScreenHeight - 1] of Char;
  r, sourceRow: Integer;
begin
  if (col < 0) or (col >= ScreenWidth) or (by <= 0) then Exit;

  by := by mod ScreenHeight; // Normalize shift amount
  if by = 0 then Exit;

  // Copy original column to tempCol
  for r := 0 to ScreenHeight - 1 do
    tempCol[r] := screen[r, col];

  // Copy back shifted
  for r := 0 to ScreenHeight - 1 do
  begin
    sourceRow := (r - by + ScreenHeight) mod ScreenHeight;
    screen[r, col] := tempCol[sourceRow];
  end;
end;

procedure ApplyInstruction(var screen: TScreen; instruction: string);
var
  p1, p2, p3, code: integer;
  s1, s2: string;
  width, height, idx, by: integer;
begin
  if Pos('rect ', instruction) = 1 then
  begin
    s1 := Copy(instruction, 6, Length(instruction) - 5); // Get 'WxH' part
    p1 := Pos('x', s1);
    if p1 > 0 then
    begin
      Val(Copy(s1, 1, p1 - 1), width, code);
      if code = 0 then
      begin
        Val(Copy(s1, p1 + 1, Length(s1)), height, code);
        if code = 0 then
          Rect(screen, width, height);
      end;
    end;
  end
  else if Pos('rotate row y=', instruction) = 1 then
  begin
    p1 := Pos('y=', instruction) + 2;
    p2 := Pos(' by ', instruction);
    if (p1 > 2) and (p2 > p1) then
    begin
       s1 := Copy(instruction, p1, p2 - p1); // Index string
       s2 := Copy(instruction, p2 + 4, Length(instruction)); // By string
       Val(s1, idx, code);
       if code = 0 then
       begin
         Val(s2, by, code);
         if code = 0 then
           RotateRow(screen, idx, by);
       end;
    end;
  end
  else if Pos('rotate column x=', instruction) = 1 then
   begin
    p1 := Pos('x=', instruction) + 2;
    p2 := Pos(' by ', instruction);
     if (p1 > 2) and (p2 > p1) then
     begin
       s1 := Copy(instruction, p1, p2 - p1); // Index string
       s2 := Copy(instruction, p2 + 4, Length(instruction)); // By string
       Val(s1, idx, code);
       if code = 0 then
       begin
         Val(s2, by, code);
         if code = 0 then
           RotateColumn(screen, idx, by);
       end;
     end;
   end;
end;

procedure PrintScreen(const screen: TScreen);
var
  r, c: Integer;
begin
  for r := 0 to ScreenHeight - 1 do
  begin
    for c := 0 to ScreenWidth - 1 do
      Write(screen[r, c]);
    WriteLn;
  end;
end;

// Main program entry point
begin
  InitializeScreen(screen);

  Assign(inputFile, 'input.txt');
  {$I-} // Disable IO checking
  Reset(inputFile);
  {$I+} // Enable IO checking
  if IOResult <> 0 then
  begin
    WriteLn('Error: Cannot open input.txt');
    Halt(1); // Exit program with error code
  end;

  while not EOF(inputFile) do
  begin
    ReadLn(inputFile, instruction);
    if Length(instruction) > 0 then // Process non-empty lines
        ApplyInstruction(screen, instruction);
  end;

  Close(inputFile);

  PrintScreen(screen);
end.
