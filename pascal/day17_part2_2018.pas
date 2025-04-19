program Flow;
uses sysutils;
const MAXW = 1500;
      MAXH = 2000;
      BASE_X = 500;
      SHIFT_X = 750;
      SHIFT_Y = 0;
      ROUND_LIM = 200000;
var
  grid: array[0..MAXH-1,0..MAXW-1] of char;
  f: text;
  s, tmp: string;
  nums: array[0..3] of integer;
  ncount, p, i, j, x, y, x1, y1, x2, y2, yv, minY, maxY, try_left, water_count: integer;
  can_move: boolean;
begin
  for i := 0 to MAXH-1 do
    for j := 0 to MAXW-1 do
      grid[i,j] := '.';
  assign(f,'input.txt');
  reset(f);
  minY := MAXH; maxY := 0;
  while not eof(f) do begin
    readln(f,s);
    ncount := 0; p := 1;
    while p <= length(s) do begin
      if s[p] in ['0'..'9'] then begin
        tmp := '';
        while (p <= length(s)) and (s[p] in ['0'..'9']) do begin
          tmp := tmp + s[p];
          inc(p);
        end;
        nums[ncount] := StrToInt(tmp);
        inc(ncount);
      end else inc(p);
    end;
    if s[1] = 'x' then begin
      x1 := nums[0]; y1 := nums[1]; y2 := nums[2];
      for y := y1 to y2 do
        grid[y-SHIFT_Y, x1-BASE_X+SHIFT_X] := '#';
      if y1 < minY then minY := y1;
      if y2 < minY then minY := y2;
      if y1 > maxY then maxY := y1;
      if y2 > maxY then maxY := y2;
    end else begin
      yv := nums[0]; x1 := nums[1]; x2 := nums[2];
      for x := x1 to x2 do
        grid[yv-SHIFT_Y, x-BASE_X+SHIFT_X] := '#';
      if yv < minY then minY := yv;
      if yv > maxY then maxY := yv;
    end;
  end;
  close(f);
  water_count := 0;
  while (grid[1,SHIFT_X] <> '|') and (water_count < ROUND_LIM) do begin
    x := SHIFT_X; y := 1; try_left := 0; can_move := true;
    while can_move do begin
      if (y+1 > maxY) or (grid[y+1,x] = '|') then begin
        grid[y,x] := '|';
        can_move := false;
      end else if grid[y+1,x] = '.' then begin
        y := y + 1;
        try_left := 0;
      end else if grid[y+1,x] in ['#','~'] then begin
        if ((try_left = 1) and (grid[y,x-1] = '|'))
        or ((try_left = 2) and (grid[y,x+1] = '|'))
        or ((grid[y,x+1] = '|') and (grid[y,x-1] <> '.'))
        or ((grid[y,x+1] <> '.') and (grid[y,x-1] = '|')) then begin
          grid[y,x] := '|';
          can_move := false;
          j := x+1;
          while grid[y,j] = '~' do begin
            grid[y,j] := '|';
            dec(water_count);
            inc(j);
          end;
          j := x-1;
          while grid[y,j] = '~' do begin
            grid[y,j] := '|';
            dec(water_count);
            dec(j);
          end;
        end else if (((try_left = 0) or (try_left = 1)) and (grid[y,x-1] = '.')) then begin
          x := x - 1;
          try_left := 1;
        end else if (((try_left = 0) or (try_left = 2)) and (grid[y,x+1] = '.')) then begin
          x := x + 1;
          try_left := 2;
        end else begin
          grid[y,x] := '~';
          inc(water_count);
          can_move := false;
        end;
      end;
    end;
  end;
  writeln(water_count);
end.
