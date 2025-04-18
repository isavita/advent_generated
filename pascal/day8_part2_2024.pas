
program ResonantCollinearity;
{$mode objfpc}{$H+}
uses
  SysUtils;

const
  MAXW = 2000;
  MAXH = 2000;
  INF = 1000000000;

type
  TPoint = record
    x, y: Integer;
  end;

var
  grid: array[0..MAXH-1] of string;
  part1mark, part2mark: array[0..MAXH-1, 0..MAXW-1] of Boolean;
  positions: array[0..61] of array of TPoint;
  W, H: Integer;

// map a frequency character to 0..61
function CharIndex(c: Char): Integer;
begin
  if (c >= '0') and (c <= '9') then
    Exit(Ord(c) - Ord('0'));
  if (c >= 'A') and (c <= 'Z') then
    Exit(10 + Ord(c) - Ord('A'));
  // 'a'..'z'
  Exit(36 + Ord(c) - Ord('a'));
end;

// gcd
function GCD(a, b: Integer): Integer;
begin
  a := Abs(a); b := Abs(b);
  while b <> 0 do
    begin
      Result := a mod b;
      a := b;
      b := Result;
    end;
  Result := a;
end;

// floor(a/b)
function FloorDiv(a, b: Integer): Integer;
var
  q, r: Integer;
begin
  q := a div b;  // trunc toward zero
  r := a mod b;
  // if a/b < 0 and mod<>0 we must decrement q
  if (r <> 0) and ((r > 0) xor (b > 0)) then
    Dec(q);
  Result := q;
end;

// ceil(a/b)
function CeilDiv(a, b: Integer): Integer;
var
  q, r: Integer;
begin
  q := a div b;
  r := a mod b;
  // if a/b > 0 and mod<>0 we must increment q
  if (r <> 0) and not((r > 0) xor (b > 0)) then
    Inc(q);
  Result := q;
end;

var
  f: Text;
  line: string;
  i, j, idx, n, p1x, p1y, p2x, p2y: Integer;
  dx, dy, g, dirX, dirY: Integer;
  low, high, k, kl, kh: Integer;
  x, y: Integer;
  part1count, part2count: Integer;
begin
  // 1) Read input.txt
  Assign(f, 'input.txt');
  Reset(f);
  H := 0;
  while not Eof(f) do
  begin
    Readln(f, line);
    if H = 0 then
      W := Length(line);
    grid[H] := line;
    Inc(H);
  end;
  Close(f);

  // 2) Collect antenna positions by frequency
  for idx := 0 to 61 do
    SetLength(positions[idx], 0);

  for y := 0 to H-1 do
    for x := 1 to W do
      if grid[y][x] <> '.' then
      begin
        idx := CharIndex(grid[y][x]);
        n := Length(positions[idx]);
        SetLength(positions[idx], n+1);
        positions[idx][n].x := x-1;
        positions[idx][n].y := y;
      end;

  // Clear mark arrays
  for y := 0 to H-1 do
    for x := 0 to W-1 do
    begin
      part1mark[y][x] := False;
      part2mark[y][x] := False;
    end;

  // 3) Part 1: for each pair of same-frequency antennas, mark the two antinodes
  for idx := 0 to 61 do
  begin
    n := Length(positions[idx]);
    if n < 2 then Continue;
    for i := 0 to n-2 do
      for j := i+1 to n-1 do
      begin
        p1x := positions[idx][i].x;
        p1y := positions[idx][i].y;
        p2x := positions[idx][j].x;
        p2y := positions[idx][j].y;
        dx := p2x - p1x;
        dy := p2y - p1y;
        // antinode before p1
        x := p1x - dx;  y := p1y - dy;
        if (x >= 0) and (x < W) and (y >= 0) and (y < H) then
          part1mark[y][x] := True;
        // antinode after p2
        x := p2x + dx;  y := p2y + dy;
        if (x >= 0) and (x < W) and (y >= 0) and (y < H) then
          part1mark[y][x] := True;
      end;
  end;

  // Count part1
  part1count := 0;
  for y := 0 to H-1 do
    for x := 0 to W-1 do
      if part1mark[y][x] then
        Inc(part1count);

  // 4) Part 2: any grid point collinear with at least two same-frequency antennas
  for idx := 0 to 61 do
  begin
    n := Length(positions[idx]);
    if n < 2 then Continue;
    for i := 0 to n-2 do
      for j := i+1 to n-1 do
      begin
        // Compute primitive direction
        p1x := positions[idx][i].x;
        p1y := positions[idx][i].y;
        p2x := positions[idx][j].x;
        p2y := positions[idx][j].y;
        dx := p2x - p1x;
        dy := p2y - p1y;
        g := GCD(dx, dy);
        dirX := dx div g;
        dirY := dy div g;

        // Find k-range so that p1 + k*(dirX,dirY) stays in [0..W-1]x[0..H-1]
        low := -INF;  high := INF;
        // X bounds
        if dirX = 0 then
        begin
          if (p1x < 0) or (p1x >= W) then
            Continue;  // line never in bounds
        end
        else if dirX > 0 then
        begin
          kl := CeilDiv(0 - p1x, dirX);
          kh := FloorDiv(W-1 - p1x, dirX);
          if kl > low then low := kl;
          if kh < high then high := kh;
        end
        else // dirX < 0
        begin
          kl := CeilDiv(W-1 - p1x, dirX);
          kh := FloorDiv(0 - p1x, dirX);
          if kl > low then low := kl;
          if kh < high then high := kh;
        end;
        if low > high then Continue;

        // Y bounds
        if dirY = 0 then
        begin
          if (p1y < 0) or (p1y >= H) then
            Continue;
        end
        else if dirY > 0 then
        begin
          kl := CeilDiv(0 - p1y, dirY);
          kh := FloorDiv(H-1 - p1y, dirY);
          if kl > low then low := kl;
          if kh < high then high := kh;
        end
        else // dirY < 0
        begin
          kl := CeilDiv(H-1 - p1y, dirY);
          kh := FloorDiv(0 - p1y, dirY);
          if kl > low then low := kl;
          if kh < high then high := kh;
        end;
        if low > high then Continue;

        // Mark every integer k in [low..high]
        for k := low to high do
        begin
          x := p1x + k*dirX;
          y := p1y + k*dirY;
          // safety check
          if (x >= 0) and (x < W) and (y >= 0) and (y < H) then
            part2mark[y][x] := True;
        end;
      end;
  end;

  // Count part2
  part2count := 0;
  for y := 0 to H-1 do
    for x := 0 to W-1 do
      if part2mark[y][x] then
        Inc(part2count);

  // Output answers
  Writeln(part1count);
  Writeln(part2count);
end.
