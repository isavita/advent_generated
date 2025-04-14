
program Solve;

{$mode delphi} // Use Delphi mode for features like dynamic arrays and Int64
{$OPTIMIZATION ON} // Enable compiler optimizations

uses
  SysUtils; // For Val, StrToIntDef, IntToStr

type
  TDisc = record
    total_positions: Integer;
    start_position: Integer;
  end;
  TDiscArray = array of TDisc;

var
  discs: TDiscArray;
  inputFile: TextFile;
  line: string;
  time: Int64;
  step: Int64;
  i, discCount: Integer;
  p1, p2: Integer;
  numStr: string;
  total, start, valCode: Integer;

begin
  AssignFile(inputFile, 'input.txt');
  try
    Reset(inputFile);

    discCount := 0;
    SetLength(discs, 0); // Initialize dynamic array

    while not Eof(inputFile) do
    begin
      ReadLn(inputFile, line);
      line := Trim(line);
      if Length(line) = 0 then Continue;

      // Extract total positions
      p1 := Pos(' has ', line);
      if p1 = 0 then Continue; // Skip invalid lines
      p1 := p1 + Length(' has ');
      p2 := Pos(' positions;', line);
      if p2 = 0 then Continue; // Skip invalid lines
      numStr := Copy(line, p1, p2 - p1);
      Val(numStr, total, valCode);
      if valCode <> 0 then Continue; // Skip if conversion fails

      // Extract start position
      p1 := Pos('position ', line);
       if p1 = 0 then Continue; // Skip invalid lines
      p1 := p1 + Length('position ');
      p2 := Pos('.', line);
       if p2 = 0 then Continue; // Skip invalid lines
       if p2 < p1 then p2 := Length(line)+1; // Handle case where '.' is last char

      numStr := Copy(line, p1, p2 - p1);
      Val(numStr, start, valCode);
      if valCode <> 0 then Continue; // Skip if conversion fails

      // Add disc to the array
      Inc(discCount);
      SetLength(discs, discCount);
      discs[discCount - 1].total_positions := total;
      discs[discCount - 1].start_position := start;
    end;
  finally
    CloseFile(inputFile);
  end;

  // Optimized search using CRT-like approach
  time := 0;
  step := 1;

  for i := 0 to High(discs) do
  begin
    // Find the first time >= current 'time' (and a multiple of 'step' away)
    // that satisfies the condition for disc 'i'
    // (disc.start_position + time + i + 1) mod disc.total_positions = 0
    while (discs[i].start_position + time + i + 1) mod discs[i].total_positions <> 0 do
    begin
      time := time + step;
    end;
    // Update the step size (using product which is sufficient here, like LCM)
    step := step * discs[i].total_positions;
  end;

  WriteLn(time);

end.
