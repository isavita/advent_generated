program NoMatterHowYouSliceIt;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

const
  SIZE = 1000;

type
  TGrid = array[0..SIZE-1, 0..SIZE-1] of Word;

var
  Grid: TGrid;

procedure Main;
var
  Lines, Parts: TStringList;
  i, x, y, w, h: Integer;
  lx, ty: Integer;
  Overlaps: LongInt;
  Line: string;
begin
  Lines := TStringList.Create;
  Parts := TStringList.Create;
  try
    // Read all claims from input.txt
    Lines.LoadFromFile('input.txt');

    // Zero the fabric grid
    for i := 0 to SIZE-1 do
      FillChar(Grid[i], SizeOf(Grid[i]), 0);

    // Parse each claim and mark the grid
    for i := 0 to Lines.Count - 1 do
    begin
      Line := Lines[i];
      Parts.Clear;
      // Split out the five numbers: ID, left, top, width, height
      ExtractStrings(['#','@',' ',',',':','x'], [], PChar(Line), Parts);
      if Parts.Count = 5 then
      begin
        // We only need the offsets and dimensions
        lx := StrToInt(Parts[1]);
        ty := StrToInt(Parts[2]);
        w  := StrToInt(Parts[3]);
        h  := StrToInt(Parts[4]);
        // Mark the covered squares
        for x := lx to lx + w - 1 do
          for y := ty to ty + h - 1 do
            Inc(Grid[x, y]);
      end;
    end;

    // Count squares with two or more claims
    Overlaps := 0;
    for x := 0 to SIZE-1 do
      for y := 0 to SIZE-1 do
        if Grid[x, y] >= 2 then
          Inc(Overlaps);

    // Output the result
    WriteLn(Overlaps);

  finally
    Parts.Free;
    Lines.Free;
  end;
end;

begin
  Main;
end.