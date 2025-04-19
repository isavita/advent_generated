program loops;
var grid: array of string;
    h,w,start_x,start_y,start_dir: integer;

function loops_sim: boolean;
var visited: array of boolean;
    dirs: array[0..3,0..1] of integer = ((0,-1),(1,0),(0,1),(-1,0));
    i,x,y,dir,dirX,dirY,nx,ny,id: longint;
begin
  SetLength(visited, w*h*4);
  fillchar(visited[0], w*h*4*SizeOf(boolean), 0);
  x := start_x; y := start_y; dir := start_dir;
  for i := 1 to 2000000 do begin
    id := ((y*w + x)*4 + dir);
    if visited[id] then begin loops_sim := True; exit; end;
    visited[id] := True;
    dirX := dirs[dir,0]; dirY := dirs[dir,1];
    nx := x + dirX; ny := y + dirY;
    if (nx < 0) or (nx >= w) or (ny < 0) or (ny >= h) then begin loops_sim := False; exit; end;
    if grid[ny][nx+1] = '#' then begin dir := (dir + 1) mod 4; continue; end;
    x := nx; y := ny;
  end;
  loops_sim := False;
end;

var s: string;
    i,j,can_loop: longint;
begin
  assign(input,'input.txt'); reset(input);
  h := 0;
  while not eof do begin
    readln(s);
    if s <> '' then begin
      inc(h);
      SetLength(grid, h);
      grid[h-1] := s;
    end;
  end;
  close(input);
  w := length(grid[0]);
  for i := 0 to h-1 do
    for j := 1 to w do
      if grid[i][j] in ['^','>','v','<'] then begin
        start_x := j-1; start_y := i;
        case grid[i][j] of
          '^': start_dir := 0;
          '>': start_dir := 1;
          'v': start_dir := 2;
          '<': start_dir := 3;
        end;
      end;
  grid[start_y][start_x+1] := '.';
  can_loop := 0;
  for i := 0 to h-1 do
    for j := 0 to w-1 do begin
      if (i = start_y) and (j = start_x) then continue;
      if grid[i][j+1] <> '.' then continue;
      grid[i][j+1] := '#';
      if loops_sim then inc(can_loop);
      grid[i][j+1] := '.';
    end;
  writeln(can_loop);
end.
