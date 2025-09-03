
program SupplyStacks;

uses
  SysUtils;

procedure Run;
var
  f: Text;
  lines: array of string;
  i, j, blankIndex, maxStack: integer;
  line, digitsLine: string;
  stacks: array of string;
  r, s, posC: integer;
  topChar: Char;
  count, fromNo, toNo: integer;
  res: string;
  valStr: string;
  idx: integer;
  lineLen: integer;
begin
  // Read input from input.txt
  assign(f, 'input.txt');
  {$I-}
  reset(f);
  {$I+}
  if IOResult <> 0 then begin
    writeln('Error: could not open input.txt');
    Exit;
  end;

  SetLength(lines, 0);
  while not Eof(f) do begin
    Readln(f, line);
    SetLength(lines, Length(lines) + 1);
    Lines[High(lines)] := line;
  end;
  Close(f);

  // Find the blank line separating drawing and moves
  blankIndex := -1;
  for i := 0 to Length(lines) - 1 do begin
    if Lines[i] = '' then begin
      blankIndex := i;
      Break;
    end;
  end;
  if blankIndex = -1 then begin
    Writeln('Invalid input: missing blank line');
    Exit;
  end;

  // Determine number of stacks from the line above the blank line
  digitsLine := Lines[blankIndex - 1];
  maxStack := 0;
  for i := 1 to Length(digitsLine) do begin
    if (digitsLine[i] >= '0') and (digitsLine[i] <= '9') then
      if (Ord(digitsLine[i]) - Ord('0')) > maxStack then
        maxStack := Ord(digitsLine[i]) - Ord('0');
  end;

  // Initialize stacks (1..maxStack). We'll ignore index 0 for convenience.
  SetLength(stacks, maxStack + 1);
  for i := 1 to maxStack do
    stacks[i] := '';

  // Build initial stacks by processing drawing lines from bottom to top
  for r := blankIndex - 2 downto 0 do begin
    line := Lines[r];
    lineLen := Length(line);
    for s := 1 to maxStack do begin
      posC := 1 + (s - 1) * 4;
      if (posC <= lineLen) and (line[posC] = '[') then begin
        topChar := line[posC + 1];
        stacks[s] := stacks[s] + topChar; // append to represent bottom->top
      end;
    end;
  end;

  // Process moves (CrateMover 9000: move one crate at a time)
  for i := blankIndex + 1 to Length(Lines) - 1 do begin
    line := Lines[i];
    if Trim(line) = '' then Continue;

    // Parse move line: "move X from Y to Z"
    count := 0; fromNo := 0; toNo := 0;
    valStr := '';
    idx := 0;
    for j := 1 to Length(line) do begin
      if (line[j] >= '0') and (line[j] <= '9') then
        valStr := valStr + line[j]
      else begin
        if valStr <> '' then begin
          inc(idx);
          case idx of
            1: count := StrToInt(valStr);
            2: fromNo := StrToInt(valStr);
            3: toNo := StrToInt(valStr);
          end;
          valStr := '';
        end;
      end;
    end;
    if valStr <> '' then begin
      inc(idx);
      case idx of
        1: count := StrToInt(valStr);
        2: fromNo := StrToInt(valStr);
        3: toNo := StrToInt(valStr);
      end;
    end;

    // Move one crate at a time
    for j := 1 to count do begin
      if Length(stacks[fromNo]) > 0 then begin
        topChar := stacks[fromNo][Length(stacks[fromNo])];
        Delete(stacks[fromNo], Length(stacks[fromNo]), 1);
        stacks[toNo] := stacks[toNo] + topChar;
      end;
    end;
  end;

  // Build result: top crate of each stack
  res := '';
  for i := 1 to maxStack do begin
    if Length(stacks[i]) > 0 then
      res := res + stacks[i][Length(stacks[i])]
  end;

  Writeln(res);
end;

begin
  Run;
end.
