
program solve;

type
  pos = record
    r, c: integer;
  end;

var
  grid: array of array of integer;
  dp: array of array of int64;
  nr, nc, r, c, i, j: integer;
  total: int64;
  dirs: array[0..3] of pos = ((r: 1; c: 0), (r: -1; c: 0), (r: 0; c: 1), (r: 0; c: -1));

function dfs(r, c: integer): int64;
var
  d: integer;
  nr2, nc2: integer;
  sum: int64;
begin
  if dp[r, c] <> -1 then
  begin
    dfs := dp[r, c];
    exit;
  end;
  if grid[r, c] = 9 then
  begin
    dp[r, c] := 1;
    dfs := 1;
    exit;
  end;
  sum := 0;
  for d := 0 to 3 do
  begin
    nr2 := r + dirs[d].r;
    nc2 := c + dirs[d].c;
    if (nr2 < 0) or (nr2 >= nr) or (nc2 < 0) or (nc2 >= nc) then
      continue;
    if grid[nr2, nc2] = grid[r, c] + 1 then
      sum := sum + dfs(nr2, nc2);
  end;
  dp[r, c] := sum;
  dfs := sum;
end;

var
  f: text;
  line: string;
begin
  assign(f, 'input.txt');
  reset(f);
  nr := 0;
  while not eof(f) do
  begin
    readln(f, line);
    inc(nr);
    if nr = 1 then
      nc := length(line);
  end;
  close(f);

  SetLength(grid, nr, nc);
  SetLength(dp, nr, nc);

  assign(f, 'input.txt');
  reset(f);
  for i := 0 to nr - 1 do
  begin
    readln(f, line);
    for j := 0 to nc - 1 do
    begin
      grid[i, j] := ord(line[j + 1]) - ord('0');
      dp[i, j] := -1;
    end;
  end;
  close(f);

  total := 0;
  for r := 0 to nr - 1 do
    for c := 0 to nc - 1 do
      if grid[r, c] = 0 then
        total := total + dfs(r, c);

  writeln(total);
end.
