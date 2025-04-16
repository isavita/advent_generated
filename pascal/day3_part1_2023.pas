
program GearRatios;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  TSchematic = array of array of Char;

function IsSymbol(c: Char): Boolean;
begin
  Result := (c <> '.') and (not (c in ['0'..'9']));
end;

function IsPartNumber(schematic: TSchematic; row, startCol, endCol: Integer): Boolean;
var
  i, j: Integer;
  numRows, numCols: Integer;
begin
  numRows := Length(schematic);
  numCols := Length(schematic[0]);

  for i := row - 1 to row + 1 do
  begin
    if (i >= 0) and (i < numRows) then
    begin
      for j := startCol - 1 to endCol + 1 do
      begin
        if (j >= 0) and (j < numCols) then
        begin
          if IsSymbol(schematic[i][j]) then
          begin
            Result := True;
            Exit;
          end;
        end;
      end;
    end;
  end;

  Result := False;
end;

function Solve: Integer;
var
  schematic: TSchematic;
  line: string;
  row, col: Integer;
  numRows, numCols: Integer;
  numberStart: Integer;
  currentNumber: string;
  partNumberSum: Integer;
  inputFile: TextFile;
begin
  AssignFile(inputFile, 'input.txt');
  Reset(inputFile);

  // Read the schematic into the 2D array
  SetLength(schematic, 0);
  row := 0;
  while not Eof(inputFile) do
  begin
    ReadLn(inputFile, line);
    SetLength(schematic, Length(schematic) + 1);
    SetLength(schematic[row], Length(line));
    for col := 0 to Length(line) - 1 do
    begin
      schematic[row][col] := line[col + 1];
    end;
    Inc(row);
  end;

  CloseFile(inputFile);

  numRows := Length(schematic);
  if numRows > 0 then
    numCols := Length(schematic[0])
  else
    numCols := 0;


  // Iterate through the schematic and find part numbers
  partNumberSum := 0;
  for row := 0 to numRows - 1 do
  begin
    col := 0;
    while col < numCols do
    begin
      if schematic[row][col] in ['0'..'9'] then
      begin
        numberStart := col;
        currentNumber := '';
        while (col < numCols) and (schematic[row][col] in ['0'..'9']) do
        begin
          currentNumber := currentNumber + schematic[row][col];
          Inc(col);
        end;

        // Check if the number is a part number
        if IsPartNumber(schematic, row, numberStart, col - 1) then
        begin
          partNumberSum := partNumberSum + StrToInt(currentNumber);
        end;
      end
      else
      begin
        Inc(col);
      end;
    end;
  end;

  Result := partNumberSum;
end;

begin
  Writeln(Solve);
end.
