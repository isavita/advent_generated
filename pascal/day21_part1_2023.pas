program BFSGrid;
uses Classes;
var
  lines: TStringList;
  rows, cols, i, j, head, tail, sx, sy, nx, ny, k, cnt: Integer;
  dist: array of array of Integer;
  qx, qy: array of Integer;
  dx: array[0..3] of Integer = (0, 0, 1, -1);
  dy: array[0..3] of Integer = (-1, 1, 0, 0);
begin
  lines := TStringList.Create;
  lines.LoadFromFile('input.txt');
  rows := lines.Count;
  cols := Length(lines[0]);
  SetLength(dist, rows, cols);
  for i := 0 to rows - 1 do
    for j := 0 to cols - 1 do
      dist[i][j] := -1;
  for i := 0 to rows - 1 do
    for j := 1 to cols do
      if lines[i][j] = 'S' then begin
        sy := i; sx := j - 1;
      end;
  SetLength(qx, rows * cols);
  SetLength(qy, rows * cols);
  head := 0; tail := 0;
  dist[sy][sx] := 0;
  qx[tail] := sx; qy[tail] := sy; Inc(tail);
  while head < tail do begin
    sx := qx[head]; sy := qy[head]; Inc(head);
    for k := 0 to 3 do begin
      nx := sx + dx[k]; ny := sy + dy[k];
      if (nx >= 0) and (ny >= 0) and (nx < cols) and (ny < rows) then
        if (lines[ny][nx+1] <> '#') and (dist[ny][nx] < 0) then begin
          dist[ny][nx] := dist[sy][sx] + 1;
          qx[tail] := nx; qy[tail] := ny; Inc(tail);
        end;
    end;
  end;
  cnt := 0;
  for i := 0 to rows - 1 do
    for j := 0 to cols - 1 do
      if (dist[i][j] >= 0) and (dist[i][j] <= 64) and (dist[i][j] mod 2 = 0) then
        Inc(cnt);
  Writeln(cnt);
end.