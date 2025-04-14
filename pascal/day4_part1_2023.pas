
program ScratchCards;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

const
  INPUT_FILE = 'input.txt';
  MAX_NUMBER = 1000; // Adjust if numbers can be larger

var
  inputFile: TextFile;
  line: string;
  totalPoints: Int64;
  pipePos: Integer;
  part1, part2: string;
  numList: TStringList;
  winningPresent: array[0..MAX_NUMBER] of Boolean;
  i, num, matches, points, code: Integer;
  numStr: string;

begin
  AssignFile(inputFile, INPUT_FILE);
  try
    Reset(inputFile);
  except
    on E: EInOutError do
    begin
      WriteLn('Error opening file: ', INPUT_FILE);
      Halt(1);
    end;
  end;

  totalPoints := 0;
  numList := TStringList.Create;
  numList.Delimiter := ' ';
  numList.StrictDelimiter := True; // Handles multiple spaces

  while not Eof(inputFile) do
  begin
    ReadLn(inputFile, line);
    pipePos := Pos('|', line);
    if pipePos > 0 then
    begin
      part1 := Trim(Copy(line, 1, pipePos - 1));
      // Skip "Card X:" prefix efficiently
      pipePos := Pos(':', part1);
      if pipePos > 0 then
         part1 := Trim(Copy(part1, pipePos + 1, Length(part1) - pipePos));

      part2 := Trim(Copy(line, Pos('|', line) + 1, Length(line)));

      // Process winning numbers using a boolean lookup for optimization
      FillChar(winningPresent, SizeOf(winningPresent), False);
      numList.DelimitedText := part1;
      for i := 0 to numList.Count - 1 do
      begin
         numStr := Trim(numList[i]);
         if numStr <> '' then
         begin
            Val(numStr, num, code);
            if (code = 0) and (num >= 0) and (num <= MAX_NUMBER) then
              winningPresent[num] := True;
         end;
      end;

      // Process your numbers and calculate points
      matches := 0;
      numList.DelimitedText := part2;
      for i := 0 to numList.Count - 1 do
      begin
         numStr := Trim(numList[i]);
         if numStr <> '' then
         begin
            Val(numStr, num, code);
            if (code = 0) and (num >= 0) and (num <= MAX_NUMBER) then
            begin
              if winningPresent[num] then
                Inc(matches);
            end;
         end;
      end;

      // Calculate points: 2^(matches-1) or 0 if no matches
      if matches > 0 then
        points := 1 shl (matches - 1) // Efficient power of 2 calculation
      else
        points := 0;

      totalPoints := totalPoints + points;
    end;
  end;

  numList.Free;
  CloseFile(inputFile);

  WriteLn(totalPoints);
end.
