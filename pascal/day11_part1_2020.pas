Program SeatingSystem;

{$mode objfpc}{$H+}

Uses
  SysUtils;

Var
  grid1, grid2: array of array of Char;
  infile: Text;
  line: string;
  H, W: Integer;
  i, j, di, dj, cnt: Integer;
  changed: Boolean;

Begin
  { Read input from "input.txt" into grid1 dynamically }
  AssignFile(infile, 'input.txt');
  Reset(infile);
  H := 0;
  W := 0;
  SetLength(grid1, 0, 0);
  While not Eof(infile) do
    Begin
      ReadLn(infile, line);
      If H = 0 Then
        W := Length(line);
      SetLength(grid1, H + 1, W);
      For j := 1 to W do
        grid1[H][j-1] := line[j];
      Inc(H);
    End;
  CloseFile(infile);

  { Prepare a second grid of the same size }
  SetLength(grid2, H, W);

  { Iterate until stabilization }
  Repeat
    changed := False;

    { Compute next state into grid2 }
    For i := 0 to H - 1 do
      For j := 0 to W - 1 do
        Begin
          If grid1[i][j] = 'L' Then
            Begin
              cnt := 0;
              For di := -1 to 1 do
                For dj := -1 to 1 do
                  If not ((di = 0) and (dj = 0)) Then
                    If (i + di >= 0) and (i + di < H)
                    and (j + dj >= 0) and (j + dj < W)
                    and (grid1[i + di][j + dj] = '#') Then
                      Inc(cnt);
              If cnt = 0 Then
                Begin
                  grid2[i][j] := '#';
                  changed := True;
                End
              Else
                grid2[i][j] := 'L';
            End
          Else If grid1[i][j] = '#' Then
            Begin
              cnt := 0;
              For di := -1 to 1 do
                For dj := -1 to 1 do
                  If not ((di = 0) and (dj = 0)) Then
                    If (i + di >= 0) and (i + di < H)
                    and (j + dj >= 0) and (j + dj < W)
                    and (grid1[i + di][j + dj] = '#') Then
                      Inc(cnt);
              If cnt >= 4 Then
                Begin
                  grid2[i][j] := 'L';
                  changed := True;
                End
              Else
                grid2[i][j] := '#';
            End
          Else
            grid2[i][j] := '.';
        End;

    { Copy new state back into grid1 }
    For i := 0 to H - 1 do
      For j := 0 to W - 1 do
        grid1[i][j] := grid2[i][j];

  Until not changed;

  { Count occupied seats in the stabilized layout }
  cnt := 0;
  For i := 0 to H - 1 do
    For j := 0 to W - 1 do
      If grid1[i][j] = '#' Then
        Inc(cnt);

  { Output the result }
  WriteLn(cnt);
End.