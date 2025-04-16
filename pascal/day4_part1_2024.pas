
program XmasCount;

{$mode objfpc}{$H+}

uses
  SysUtils;

const
  FileName = 'input.txt';
  Word = 'XMAS';
  L = 4;
  Dirs: array[0..7, 0..1] of Integer = (
    (1, 0), (-1, 0), (0, 1), (0, -1),
    (1, 1), (1, -1), (-1, 1), (-1, -1)
  );

var
  Grid: array of string;
  R, C, Count, I, J, Dx, Dy, X, Y, K, Dir: Integer;
  Line: string;
  F: TextFile;

begin
  SetLength(Grid, 0);
  AssignFile(F, FileName);
  Reset(F);
  while not Eof(F) do
  begin
    ReadLn(F, Line);
    SetLength(Grid, Length(Grid) + 1);
    Grid[High(Grid)] := Line;
  end;
  CloseFile(F);

  R := Length(Grid);
  if R > 0 then
    C := Length(Grid[0])
  else
    C := 0;

  Count := 0;
  for I := 0 to R - 1 do
  begin
    for J := 0 to C - 1 do
    begin
      if (C > 0) and (Grid[I][J + 1] = Word[1]) then
      begin
        for Dir := 0 to 7 do
        begin
          Dx := Dirs[Dir, 0];
          Dy := Dirs[Dir, 1];
          X := I;
          Y := J;
          K := 1;

          while (X + Dx >= 0) and (X + Dx < R) and (Y + Dy >= 0) and (Y + Dy < C) and (K < L) and (Grid[X + Dx][Y + Dy + 1] = Word[K + 1]) do
          begin
            X := X + Dx;
            Y := Y + Dy;
            Inc(K);
          end;

          if K = L then
          begin
            Inc(Count);
          end;
        end;
      end;
    end;
  end;

  WriteLn(Count);
end.
