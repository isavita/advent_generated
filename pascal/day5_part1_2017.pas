program MazeOfTwistyTrampolines;
{$mode objfpc}{$H+}
uses
  SysUtils, Classes;

var
  Offsets: array of Integer;
  Lines: TStringList;
  i, Count, Pos, Steps, Jump: Integer;

begin
  { Read all offsets from input.txt into a string list }
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile('input.txt');
    Count := Lines.Count;
    
    { Transfer them into a dynamic integer array }
    SetLength(Offsets, Count);
    for i := 0 to Count - 1 do
      Offsets[i] := StrToInt(Trim(Lines[i]));

    { Walk the maze until we jump outside the array bounds }
    Pos := 0;
    Steps := 0;
    while (Pos >= 0) and (Pos < Count) do
    begin
      Jump := Offsets[Pos];        { current offset }
      Offsets[Pos] := Jump + 1;    { increment it }
      Inc(Pos, Jump);              { perform the jump }
      Inc(Steps);                  { count this step }
    end;

    { Output the number of steps taken to exit }
    Writeln(Steps);
  finally
    Lines.Free;
  end;
end.