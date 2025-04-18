
program LobbyLayout;
{$mode objfpc}{$H+}
uses
  SysUtils, Classes;

var
  Tiles: TStringList;
  Line: string;
  i, idx, x, y: Integer;
  key: string;
begin
  Tiles := TStringList.Create;
  try
    // keep it sorted so IndexOf is binary‐search
    Tiles.Sorted := True;
    Tiles.Duplicates := dupIgnore;

    AssignFile(Input, 'input.txt');
    Reset(Input);
    try
      while not EOF(Input) do
      begin
        ReadLn(Input, Line);
        x := 0; y := 0;
        i := 1;
        // parse a sequence of e, se, sw, w, nw, ne
        while i <= Length(Line) do
        begin
          case Line[i] of
            'e': begin
                   x := x + 2;
                   Inc(i);
                 end;
            'w': begin
                   x := x - 2;
                   Inc(i);
                 end;
            'n': begin
                   // ne or nw
                   if (i < Length(Line)) and (Line[i+1] = 'e') then
                   begin
                     x := x + 1;
                     y := y + 1;
                   end
                   else
                   begin
                     x := x - 1;
                     y := y + 1;
                   end;
                   Inc(i, 2);
                 end;
            's': begin
                   // se or sw
                   if (i < Length(Line)) and (Line[i+1] = 'e') then
                   begin
                     x := x + 1;
                     y := y - 1;
                   end
                   else
                   begin
                     x := x - 1;
                     y := y - 1;
                   end;
                   Inc(i, 2);
                 end;
          else
            // shouldn't happen
            Inc(i);
          end;
        end;

        // build a unique key for this tile
        key := IntToStr(x) + ',' + IntToStr(y);
        idx := Tiles.IndexOf(key);
        if idx >= 0 then
          // was black, flip back to white
          Tiles.Delete(idx)
        else
          // was white, flip to black
          Tiles.Add(key);
      end;
    finally
      CloseFile(Input);
    end;

    // remaining entries are black tiles
    WriteLn(Tiles.Count);
  finally
    Tiles.Free;
  end;
end.
