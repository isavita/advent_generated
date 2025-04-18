Program CosmicExpansion;
{$APPTYPE CONSOLE}
Uses
  SysUtils;

Var
  F: Text;
  inputLines: array of string;
  line: string;
  R0, C0, i, j: Integer;
  rowHas, rowExpand: array of Boolean;
  colHas, colExpand: array of Boolean;
  R2, C2: Integer;
  grid: array of array of Boolean;
  rr, cc, k: Integer;
  gi, gj: array of Integer;
  G: Integer;
  dist: array of array of Integer;
  qx, qy: array of Integer;
  head, tail: Integer;
  di: array[0..3] of Integer = (-1, 1, 0, 0);
  dj: array[0..3] of Integer = (0, 0, -1, 1);
  srcI, srcJ, uI, uJ, vI, vJ: Integer;
  gIdx, h: Integer;
  totalSum: Int64;

Begin
  Assign(F, 'input.txt');
  Reset(F);
  R0 := 0;
  While not Eof(F) Do
  Begin
    Readln(F, line);
    SetLength(inputLines, R0+1);
    inputLines[R0] := line;
    Inc(R0);
  End;
  Close(F);
  If R0 = 0 Then
  Begin
    Writeln(0);
    Halt;
  End;
  C0 := Length(inputLines[0]);
  SetLength(rowHas, R0);
  SetLength(rowExpand, R0);
  SetLength(colHas, C0);
  SetLength(colExpand, C0);
  // mark rows/cols that have a galaxy '#'
  For i := 0 To R0-1 Do
    For j := 0 To C0-1 Do
      If inputLines[i][j+1] = '#' Then
      Begin
        rowHas[i] := True;
        colHas[j] := True;
      End;
  // rows/cols with no galaxy must expand
  For i := 0 To R0-1 Do
    If Not rowHas[i] Then rowExpand[i] := True;
  For j := 0 To C0-1 Do
    If Not colHas[j] Then colExpand[j] := True;
  // compute expanded dimensions
  R2 := 0;
  For i := 0 To R0-1 Do
  Begin
    Inc(R2);
    If rowExpand[i] Then Inc(R2);
  End;
  C2 := 0;
  For j := 0 To C0-1 Do
  Begin
    Inc(C2);
    If colExpand[j] Then Inc(C2);
  End;
  // build expanded grid
  SetLength(grid, R2, C2);
  rr := 0;
  For i := 0 To R0-1 Do
  Begin
    cc := 0;
    For j := 0 To C0-1 Do
    Begin
      grid[rr, cc] := (inputLines[i][j+1] = '#');
      Inc(cc);
      If colExpand[j] Then
      Begin
        grid[rr, cc] := grid[rr, cc-1];
        Inc(cc);
      End;
    End;
    Inc(rr);
    If rowExpand[i] Then
    Begin
      // duplicate the whole row
      For k := 0 To C2-1 Do
        grid[rr, k] := grid[rr-1, k];
      Inc(rr);
    End;
  End;
  // collect galaxy coordinates
  G := 0;
  For i := 0 To R2-1 Do
    For j := 0 To C2-1 Do
      If grid[i, j] Then
      Begin
        Inc(G);
        SetLength(gi, G);
        SetLength(gj, G);
        gi[G-1] := i;
        gj[G-1] := j;
      End;
  // prepare for BFS
  SetLength(dist, R2, C2);
  SetLength(qx, R2*C2);
  SetLength(qy, R2*C2);
  totalSum := 0;
  // BFS from each galaxy, summing distances to galaxies with higher index
  For gIdx := 0 To G-1 Do
  Begin
    // init distances
    For i := 0 To R2-1 Do
      For j := 0 To C2-1 Do
        dist[i][j] := -1;
    // start BFS
    srcI := gi[gIdx];
    srcJ := gj[gIdx];
    head := 0; tail := 0;
    dist[srcI][srcJ] := 0;
    qx[tail] := srcI;
    qy[tail] := srcJ;
    Inc(tail);
    While head < tail Do
    Begin
      uI := qx[head];
      uJ := qy[head];
      Inc(head);
      For k := 0 To 3 Do
      Begin
        vI := uI + di[k];
        vJ := uJ + dj[k];
        If (vI >= 0) And (vI < R2) And (vJ >= 0) And (vJ < C2) Then
          If (dist[vI][vJ] < 0) Then
          Begin
            dist[vI][vJ] := dist[uI][uJ] + 1;
            qx[tail] := vI;
            qy[tail] := vJ;
            Inc(tail);
          End;
      End;
    End;
    // accumulate to galaxies with index > gIdx
    For h := gIdx+1 To G-1 Do
      totalSum := totalSum + dist[ gi[h] ][ gj[h] ];
  End;
  // output result
  Writeln(totalSum);
End.