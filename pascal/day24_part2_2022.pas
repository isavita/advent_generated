program PathBlizzards;

{$mode objfpc}{$H+}

uses
  SysUtils;

const
  MAX_H = 200;
  MAX_W = 200;
  MAX_BLIZZARDS = 10000;
  MAX_PERIOD = 30000;
  MAX_QUEUE_SIZE = 4000000;

type
  TPoint = record
    x, y: Integer;
  end;
  TBlizzard = record
    x, y, dir_idx: Integer;
  end;
  TState = record
    x, y, t: Integer;
  end;

var
  walls: array[0..MAX_H-1, 0..MAX_W-1] of Boolean;
  height, width: Integer;
  start_pos, end_pos: TPoint;

  initial_blizzards: array[0..MAX_BLIZZARDS-1] of TBlizzard;
  num_blizzards: Integer;

  blizzard_positions: array of array of Boolean; // per time t: 1D grid [y*width + x]
  period: Integer;

  queue: array[0..MAX_QUEUE_SIZE-1] of TState;
  queue_head, queue_tail: Integer;

  visited: array of Boolean;

  DX: array[0..4] of Integer = (0, 1, -1, 0, 0);
  DY: array[0..4] of Integer = (0, 0, 0, 1, -1);

function gcd(a, b: Integer): Integer;
var
  aa, bb, tmp: Integer;
begin
  aa := a;
  bb := b;
  while bb <> 0 do
  begin
    tmp := aa mod bb;
    aa := bb;
    bb := tmp;
  end;
  Result := aa;
end;

function lcm(a, b: Integer): Integer;
var
  g: Integer;
  v: Int64;
begin
  if (a = 0) or (b = 0) then Exit(0);
  g := gcd(a, b);
  v := (Int64(a) div g) * b;
  if v > MAX_PERIOD then v := MAX_PERIOD;
  Result := Integer(v);
end;

function pos_mod(i, n: Integer): Integer;
var
  r: Integer;
begin
  r := i mod n;
  if r < 0 then r := r + n;
  Result := r;
end;

procedure ReadInput(const FileName: String);
var
  f: TextFile;
  line: String;
  x, y: Integer;
  ch: Char;
begin
  for y := 0 to MAX_H - 1 do
    for x := 0 to MAX_W - 1 do walls[y, x] := False;

  height := 0;
  width := 0;
  num_blizzards := 0;

  AssignFile(f, FileName);
  Reset(f);
  try
    while not Eof(f) do
    begin
      ReadLn(f, line);
      if width = 0 then
        width := Length(line);
      for x := 0 to width - 1 do
      begin
        ch := line[x + 1];
        if ch = '#' then
          walls[height, x] := True
        else if ch <> '.' then
        begin
          initial_blizzards[num_blizzards].x := x;
          initial_blizzards[num_blizzards].y := height;
          case ch of
            '>': initial_blizzards[num_blizzards].dir_idx := 0;
            '<': initial_blizzards[num_blizzards].dir_idx := 1;
            'v': initial_blizzards[num_blizzards].dir_idx := 2;
            '^': initial_blizzards[num_blizzards].dir_idx := 3;
          end;
          Inc(num_blizzards);
        end;
      end;
      Inc(height);
      if height > MAX_H then Halt(1);
    end;
  finally
    CloseFile(f);
  end;

  if (width <= 2) or (height <= 2) then Halt(1);
end;

procedure FindStartEnd;
var
  x: Integer;
begin
  for x := 0 to width - 1 do
    if not walls[0, x] then
    begin
      start_pos.x := x;
      start_pos.y := 0;
      Break;
    end;

  for x := 0 to width - 1 do
    if not walls[height - 1, x] then
    begin
      end_pos.x := x;
      end_pos.y := height - 1;
      Break;
    end;
end;

procedure PrecomputeBlizzards;
var
  innerW, innerH, t, i, nx, ny: Integer;
  s: Integer;
begin
  innerW := width - 2;
  innerH := height - 2;
  period := lcm(innerW, innerH);
  if period = 0 then period := 1;

  SetLength(blizzard_positions, period);
  for t := 0 to period - 1 do
  begin
    SetLength(blizzard_positions[t], height * width);
    FillChar(blizzard_positions[t][0], height * width * SizeOf(Boolean), 0);
  end;

  for t := 0 to period - 1 do
  begin
    for i := 0 to num_blizzards - 1 do
    begin
      nx := initial_blizzards[i].x;
      ny := initial_blizzards[i].y;
      case initial_blizzards[i].dir_idx of
        0: nx := 1 + pos_mod(initial_blizzards[i].x - 1 + t, innerW);
        1: nx := 1 + pos_mod(initial_blizzards[i].x - 1 - t, innerW);
        2: ny := 1 + pos_mod(initial_blizzards[i].y - 1 + t, innerH);
        3: ny := 1 + pos_mod(initial_blizzards[i].y - 1 - t, innerH);
      end;
      if (ny >= 0) and (ny < height) and (nx >= 0) and (nx < width) then
        blizzard_positions[t][ny * width + nx] := True;
    end;
  end;
end;

function BFS(startS, endS: TPoint; start_time: Integer): Integer;
var
  i, nx, ny, next_t, next_mod, idx: Integer;
  state: TState;
  newState: TState;
  endReached: Boolean;
  sIndex: Integer;
begin
  SetLength(visited, height * width * period);
  FillChar(visited[0], Length(visited) * SizeOf(Boolean), 0);

  queue_head := 0;
  queue_tail := 0;

  state.x := startS.x;
  state.y := startS.y;
  state.t := start_time;
  queue[queue_tail] := state;
  Inc(queue_tail);

  sIndex := ((state.y * width) + state.x) * period + (start_time mod period);
  visited[sIndex] := True;

  endReached := False;

  while queue_head < queue_tail do
  begin
    state := queue[queue_head];
    Inc(queue_head);

    for i := 0 to 4 do
    begin
      nx := state.x + DX[i];
      ny := state.y + DY[i];

      if (nx < 0) or (nx >= width) or (ny < 0) or (ny >= height) then Continue;
      if walls[ny, nx] then Continue;

      if (nx = endS.x) and (ny = endS.y) then
      begin
        Result := state.t + 1;
        Exit;
      end;

      next_t := state.t + 1;
      next_mod := next_t mod period;

      if blizzard_positions[next_mod][ny * width + nx] then Continue;

      idx := ((ny * width) + nx) * period + next_mod;
      if visited[idx] then Continue;

      visited[idx] := True;
      newState.x := nx;
      newState.y := ny;
      newState.t := next_t;
      queue[queue_tail] := newState;
      Inc(queue_tail);

      if queue_tail > MAX_QUEUE_SIZE then
      begin
        WriteLn('Error: BFS Queue overflow');
        Result := -1;
        Exit;
      end;
    end;
  end;

  SetLength(visited, 0);
  Result := -1;
end;

var
  time1, time2, time3: Integer;
  t1, t2, t3: Integer;

begin
  ReadInput('input.txt');
  FindStartEnd;
  PrecomputeBlizzards;

  time1 := BFS(start_pos, end_pos, 0);
  time2 := BFS(end_pos, start_pos, time1);
  time3 := BFS(start_pos, end_pos, time2);

  WriteLn(time3);

  SetLength(blizzard_positions, 0);
end.