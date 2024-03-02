program Day3Part2;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

var
  instructions: String;
  visitedHouses: TStringList;
  xSanta, ySanta, xRoboSanta, yRoboSanta, i: Integer;
  currentPos: String;
  inputFile: TextFile;

function PosToStr(x, y: Integer): String;
begin
  Result := IntToStr(x) + ',' + IntToStr(y);
end;

begin
  // Initial positions
  xSanta := 0; ySanta := 0;
  xRoboSanta := 0; yRoboSanta := 0;

  visitedHouses := TStringList.Create;
  try
    visitedHouses.Duplicates := dupIgnore; // Ignore duplicate entries
    visitedHouses.Sorted := True; // Sorting for faster search

    // Adding the starting position as visited
    visitedHouses.Add(PosToStr(0, 0));

    // Load the instructions from file
    AssignFile(inputFile, 'input.txt');
    Reset(inputFile);
    ReadLn(inputFile, instructions);
    CloseFile(inputFile);

    for i := 1 to Length(instructions) do
    begin
      // Update positions for Santa and Robo-Santa in turns
      if i mod 2 = 1 then // Odd instructions for Santa
      begin
        case instructions[i] of
          '^': Inc(ySanta);
          'v': Dec(ySanta);
          '>': Inc(xSanta);
          '<': Dec(xSanta);
        end;
        currentPos := PosToStr(xSanta, ySanta);
      end
      else // Even instructions for Robo-Santa
      begin
        case instructions[i] of
          '^': Inc(yRoboSanta);
          'v': Dec(yRoboSanta);
          '>': Inc(xRoboSanta);
          '<': Dec(xRoboSanta);
        end;
        currentPos := PosToStr(xRoboSanta, yRoboSanta);
      end;

      // Mark the new position as visited
      visitedHouses.Add(currentPos);
    end;

    // Output the total number of houses that received at least one present
    WriteLn('Total houses that received at least one present: ', visitedHouses.Count);
  finally
    visitedHouses.Free;
  end;
end.
