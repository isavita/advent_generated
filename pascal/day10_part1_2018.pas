program StarsAlign;

uses
  SysUtils;

type
  PRec = record
    x, y, vx, vy: Int64;
  end;

var
  pts: array of PRec;
  n: integer;

  f: Text;
  line: string;
  a,b,c,d: Int64;

  bestT, t: Int64;
  mnX, mxX, mnY, mxY: Int64;
  area, prevArea: Int64;
  w, h: integer;
  grid: array of array of boolean;
  i, ix, iy: integer;

procedure parseLine(s: string; var x,y,vx,vy: Int64);
  var idx: integer;
      function nextInt(): Int64;
      var sign: integer;
          val: Int64;
      begin
        sign := 1;
        while (idx <= length(s)) and not (s[idx] in ['-','0'..'9']) do inc(idx);
        if idx > length(s) then begin nextInt := 0; exit; end;
        if s[idx] = '-' then begin sign := -1; inc(idx); end;
        val := 0;
        while (idx <= length(s)) and (s[idx] in ['0'..'9']) do begin
          val := val*10 + (Ord(s[idx]) - Ord('0'));
          inc(idx);
        end;
        nextInt := sign * val;
      end;
  begin
    idx := 1;
    x := nextInt();
    y := nextInt();
    vx := nextInt();
    vy := nextInt();
  end;

procedure calcBounds(t: Int64; var mnX, mxX, mnY, mxY: Int64);
var i: integer;
    xi, yi: Int64;
begin
  mnX := High(Int64); mxX := Low(Int64);
  mnY := High(Int64); mxY := Low(Int64);
  for i := 0 to n-1 do begin
    xi := pts[i].x + pts[i].vx * t;
    yi := pts[i].y + pts[i].vy * t;
    if xi < mnX then mnX := xi;
    if xi > mxX then mxX := xi;
    if yi < mnY then mnY := yi;
    if yi > mxY then mxY := yi;
  end;
end;

begin
  SetLength(pts, 0);
  n := 0;
  assign(f, 'input.txt');
  Reset(f);
  while not Eof(f) do begin
    ReadLn(f, line);
    inc(n);
    SetLength(pts, n);
    parseLine(line, a, b, c, d);
    pts[n-1].x := a;
    pts[n-1].y := b;
    pts[n-1].vx := c;
    pts[n-1].vy := d;
  end;
  Close(f);

  if n = 0 then
  begin
    Exit;
  end;

  calcBounds(0, mnX, mxX, mnY, mxY);
  prevArea := (mxX - mnX) * (mxY - mnY);
  bestT := 0;
  t := 1;
  while true do begin
    calcBounds(t, mnX, mxX, mnY, mxY);
    area := (mxX - mnX) * (mxY - mnY);
    if area < prevArea then begin
      bestT := t;
    end;
    if area > prevArea then Break;
    prevArea := area;
    Inc(t);
    if t > 200000 then Break;
  end;

  // Build and print the message
  calcBounds(bestT, mnX, mxX, mnY, mxY);
  w := Integer(mxX - mnX + 1);
  h := Integer(mxY - mnY + 1);
  if (w > 0) and (h > 0) then begin
    SetLength(grid, w);
    for i := 0 to w - 1 do
      SetLength(grid[i], h);
    // initialize to false
    for ix := 0 to w - 1 do
      for iy := 0 to h - 1 do
        grid[ix][iy] := False;
    for i := 0 to n - 1 do begin
      ix := Integer(pts[i].x + pts[i].vx * bestT - mnX);
      iy := Integer(pts[i].y + pts[i].vy * bestT - mnY);
      if (ix >= 0) and (ix < w) and (iy >= 0) and (iy < h) then
        grid[ix][iy] := True;
    end;
    for iy := 0 to h - 1 do begin
      for ix := 0 to w - 1 do begin
        if grid[ix][iy] then write('#') else write('.');
      end;
      writeln;
    end;
  end;
end.