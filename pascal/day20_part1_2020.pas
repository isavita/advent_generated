Program JurassicJigsaw;
{$mode objfpc}{$H+}
Uses
  SysUtils;

Type
  TTile = record
    id: LongInt;
    edgesNorm: array[0..3] of String;
  end;

Var
  tiles: array of TTile;
  edgeList: array of String;
  edgeCount: array of LongInt;
  totalEdges: LongInt = 0;

Function ReverseStr(const S: String): String;
Var
  i: Integer;
Begin
  SetLength(Result, Length(S));
  For i := 1 to Length(S) do
    Result[i] := S[Length(S) - i + 1];
End;

Function NormalizeEdge(const E: String): String;
Var
  R: String;
Begin
  R := ReverseStr(E);
  If E < R then
    Result := E
  Else
    Result := R;
End;

Procedure AddEdge(const E: String);
Var
  i: LongInt;
Begin
  For i := 0 to totalEdges - 1 do
    If edgeList[i] = E then
    Begin
      Inc(edgeCount[i]);
      Exit;
    End;
  // new edge
  SetLength(edgeList, totalEdges + 1);
  SetLength(edgeCount, totalEdges + 1);
  edgeList[totalEdges] := E;
  edgeCount[totalEdges] := 1;
  Inc(totalEdges);
End;

Function FindEdgeIndex(const E: String): LongInt;
Var
  i: LongInt;
Begin
  For i := 0 to totalEdges - 1 do
    If edgeList[i] = E then
      Exit(i);
  Result := -1;
End;

Var
  InF: TextFile;
  line: String;
  block: array of String;
  i, m: Integer;
  tile: TTile;
  rawLines: array of String;
  topE, bottomE, leftE, rightE, tmp: String;
  r: QWord;
  cornerEdges: Integer;
  idx: LongInt;
Begin
  // Read input.txt
  AssignFile(InF, 'input.txt');
  Reset(InF);
  SetLength(block, 0);
  While not Eof(InF) do
  Begin
    ReadLn(InF, line);
    If line = '' then
    Begin
      // process block
      If Length(block) > 0 then
      Begin
        // parse tile ID
        tmp := block[0]; // "Tile 1234:"
        Delete(tmp, 1, 5);
        Delete(tmp, Length(tmp), 1);
        Val(tmp, tile.id, i);
        // prepare raw lines
        m := Length(block) - 1;
        SetLength(rawLines, m);
        For i := 1 to m do
          rawLines[i - 1] := block[i];
        // extract edges
        topE := rawLines[0];
        bottomE := rawLines[m - 1];
        leftE := '';
        rightE := '';
        For i := 0 to m - 1 do
        Begin
          leftE += rawLines[i][1];
          rightE += rawLines[i][Length(rawLines[i])];
        End;
        // normalize and store
        tile.edgesNorm[0] := NormalizeEdge(topE);
        tile.edgesNorm[1] := NormalizeEdge(bottomE);
        tile.edgesNorm[2] := NormalizeEdge(leftE);
        tile.edgesNorm[3] := NormalizeEdge(rightE);
        // add tile
        SetLength(tiles, Length(tiles) + 1);
        tiles[High(tiles)] := tile;
        // add edges to global
        For i := 0 to 3 do
          AddEdge(tile.edgesNorm[i]);
      End;
      // reset block
      SetLength(block, 0);
    End
    Else
    Begin
      // accumulate lines
      SetLength(block, Length(block) + 1);
      block[High(block)] := line;
    End;
  End;
  // last block if any
  If Length(block) > 0 then
  Begin
    tmp := block[0];
    Delete(tmp, 1, 5);
    Delete(tmp, Length(tmp), 1);
    Val(tmp, tile.id, i);
    m := Length(block) - 1;
    SetLength(rawLines, m);
    For i := 1 to m do
      rawLines[i - 1] := block[i];
    topE := rawLines[0];
    bottomE := rawLines[m - 1];
    leftE := '';
    rightE := '';
    For i := 0 to m - 1 do
    Begin
      leftE += rawLines[i][1];
      rightE += rawLines[i][Length(rawLines[i])];
    End;
    tile.edgesNorm[0] := NormalizeEdge(topE);
    tile.edgesNorm[1] := NormalizeEdge(bottomE);
    tile.edgesNorm[2] := NormalizeEdge(leftE);
    tile.edgesNorm[3] := NormalizeEdge(rightE);
    SetLength(tiles, Length(tiles) + 1);
    tiles[High(tiles)] := tile;
    For i := 0 to 3 do
      AddEdge(tile.edgesNorm[i]);
  End;
  CloseFile(InF);

  // find corner tiles
  r := 1;
  For i := 0 to High(tiles) do
  Begin
    cornerEdges := 0;
    For idx := 0 to 3 do
      If edgeCount[FindEdgeIndex(tiles[i].edgesNorm[idx])] = 1 then
        Inc(cornerEdges);
    If cornerEdges = 2 then
      r := r * QWord(tiles[i].id);
  End;

  // output result
  WriteLn(r);
End.

{ 
  The program reads tiles from "input.txt", extracts each tile's four borders,
  normalizes them (so that an edge and its reverse are considered identical),
  counts how many tiles share each unique border, and identifies corner tiles
  as those with exactly two borders that no other tile shares. It multiplies
  the IDs of those corner tiles and prints the product.
}