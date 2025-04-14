
program Solve;

const
  MaxN = 1001; // Max grid dimension + buffer
  MaxQueueSize = MaxN * MaxN;
  MaxTrackCells = MaxN * MaxN;

type
  TPoint = record
    r, c: Integer;
  end;
  TGrid = array[0..MaxN-1] of string;
  TDistGrid = array[0..MaxN-1, 0..MaxN-1] of Integer;
  TBoolGrid = array[0..MaxN-1, 0..MaxN-1] of Boolean;
  TPointArray = array[1..MaxTrackCells] of TPoint;
  TQueue = array[1..MaxQueueSize] of TPoint;
  TDirArray = array[1..4] of TPoint;

var
  grid: TGrid;
  walls: TBoolGrid;
  dist_from_s, dist_from_e: TDistGrid;
  track_cells: TPointArray;
  queue: TQueue;
  S, E: TPoint;
  h, w, track_count: Integer;
  qHead, qTail: Integer;
  dirs: TDirArray;
  inputFile: Text;

procedure InitializeDistGrid(var dist: TDistGrid);
var
  r, c: Integer;
begin
  for r := 0 to h - 1 do
    for c := 0 to w - 1 do
      dist[r, c] := -1;
end;

procedure BFS(startR, startC: Integer; var dist: TDistGrid);
var
  r, c, nr, nc, i: Integer;
  curr: TPoint;
  dr, dc: Integer;
begin
  InitializeDistGrid(dist);

  qHead := 1;
  qTail := 0;

  qTail := qTail + 1;
  queue[qTail].r := startR;
  queue[qTail].c := startC;
  dist[startR, startC] := 0;

  while qHead <= qTail do
  begin
    curr := queue[qHead];
    qHead := qHead + 1;
    r := curr.r;
    c := curr.c;

    for i := 1 to 4 do
    begin
      dr := dirs[i].r;
      dc := dirs[i].c;
      nr := r + dr;
      nc := c + dc;

      if (nr >= 0) and (nr < h) and (nc >= 0) and (nc < w) then
      begin
        if not walls[nr, nc] then
        begin
          if dist[nr, nc] = -1 then
          begin
            dist[nr, nc] := dist[r, c] + 1;
            qTail := qTail + 1;
            queue[qTail].r := nr;
            queue[qTail].c := nc;
          end;
        end;
      end;
    end;
  end;
end;

var
  r, c, i, d1, d2: Integer;
  grid_row: string;
  start_pos: TPoint;
  m1r, m1c, m2r, m2c, sd, ed, new_cost, saving, normal_cost, possible_cheats: Integer;

begin
  Assign(inputFile, 'input.txt');
  Reset(inputFile);

  h := 0;
  track_count := 0;
  while not Eof(inputFile) do
  begin
    ReadLn(inputFile, grid_row);
    if h = 0 then
      w := Length(grid_row);
    grid[h] := grid_row;

    for c := 0 to w - 1 do
    begin
       walls[h, c] := False; // Initialize walls for the row
       if grid_row[c + 1] = 'S' then
       begin
         S.r := h;
         S.c := c;
         track_count := track_count + 1;
         track_cells[track_count].r := h;
         track_cells[track_count].c := c;
       end
       else if grid_row[c + 1] = 'E' then
       begin
         E.r := h;
         E.c := c;
         track_count := track_count + 1;
         track_cells[track_count].r := h;
         track_cells[track_count].c := c;
       end
       else if grid_row[c + 1] = '#' then
       begin
         walls[h, c] := True;
       end
       else // '.' or other non-wall characters
       begin
         track_count := track_count + 1;
         track_cells[track_count].r := h;
         track_cells[track_count].c := c;
       end;
    end;
    Inc(h);
  end;
  Close(inputFile);

  dirs[1].r := 1; dirs[1].c := 0;
  dirs[2].r := -1; dirs[2].c := 0;
  dirs[3].r := 0; dirs[3].c := 1;
  dirs[4].r := 0; dirs[4].c := -1;

  BFS(S.r, S.c, dist_from_s);
  BFS(E.r, E.c, dist_from_e);

  normal_cost := dist_from_s[E.r, E.c];

  if normal_cost = -1 then
  begin
    WriteLn(0);
    Halt;
  end;

  possible_cheats := 0;
  for i := 1 to track_count do
  begin
    start_pos := track_cells[i];
    sd := dist_from_s[start_pos.r, start_pos.c];

    if sd <> -1 then
    begin
      for d1 := 1 to 4 do
      begin
        m1r := start_pos.r + dirs[d1].r;
        m1c := start_pos.c + dirs[d1].c;

        if (m1r >= 0) and (m1r < h) and (m1c >= 0) and (m1c < w) then
        begin
          for d2 := 1 to 4 do
          begin
            m2r := m1r + dirs[d2].r;
            m2c := m1c + dirs[d2].c;

            if (m2r >= 0) and (m2r < h) and (m2c >= 0) and (m2c < w) then
            begin
              if not walls[m2r, m2c] then
              begin
                ed := dist_from_e[m2r, m2c];
                if ed <> -1 then
                begin
                  new_cost := sd + 2 + ed;
                  saving := normal_cost - new_cost;
                  if saving >= 100 then
                  begin
                    Inc(possible_cheats);
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  WriteLn(possible_cheats);
end.
