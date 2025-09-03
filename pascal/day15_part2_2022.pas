
program BeaconExclusionZone;

{ Reads input from input.txt and prints two lines:
   - Part 1: number of positions in row y=2000000 that cannot contain a beacon
   - Part 2: tuning frequency of the only possible distress beacon
}

uses
  SysUtils;

type
  TPoint = record
    x: Int64;
    y: Int64;
  end;

  TSensor = record
    s: TPoint;   { sensor position }
    b: TPoint;   { closest beacon position }
    r: Int64;     { Manhattan distance from sensor to closest beacon (radius) }
  end;

  TInterval = record
    l: Int64;
    r: Int64;
  end;

  TInt64Array = array of Int64;

function Abs64(v: Int64): Int64;
begin
  if v < 0 then Abs64 := -v else Abs64 := v;
end;

procedure ExtractIntsFromLine(line: string; var vals: TInt64Array);
var
  i, n, len: Integer;
  ch: Char;
  sign: Int64;
  cur: Int64;
begin
  SetLength(vals, 0);
  len := Length(line);
  i := 1;
  while i <= len do
  begin
    ch := line[i];
    if (ch = '-') or ((ch >= '0') and (ch <= '9')) then
    begin
      if ch = '-' then
      begin
        sign := -1;
        inc(i);
        cur := 0;
        while (i <= len) and (line[i] >= '0') and (line[i] <= '9') do
        begin
          cur := cur * 10 + (Ord(line[i]) - Ord('0'));
          inc(i);
        end;
        SetLength(vals, Length(vals) + 1);
        vals[Length(vals) - 1] := sign * cur;
      end
      else
      begin
        sign := 1;
        cur := 0;
        while (i <= len) and (line[i] >= '0') and (line[i] <= '9') do
        begin
          cur := cur * 10 + (Ord(line[i]) - Ord('0'));
          inc(i);
        end;
        SetLength(vals, Length(vals) + 1);
        vals[Length(vals) - 1] := sign * cur;
      end;
    end
    else
      inc(i);
  end;
end;

procedure SortIntervalsByL(var a: array of TInterval);
var
  i, j: Integer;
  tmp: TInterval;
begin
  for i := 1 to High(a) do
  begin
    tmp := a[i];
    j := i - 1;
    while (j >= 0) and (a[j].l > tmp.l) do
    begin
      a[j + 1] := a[j];
      Dec(j);
    end;
    a[j + 1] := tmp;
  end;
end;

function PointInUnion(x: Int64; const inters: array of TInterval): Boolean;
var
  i: Integer;
begin
  for i := 0 to High(inters) do
  begin
    if (x >= inters[i].l) and (x <= inters[i].r) then
    begin
      PointInUnion := True;
      Exit;
    end;
  end;
  PointInUnion := False;
end;

var
  sensors: array of TSensor;
  N, i, j: Integer;
  line: string;
  f: Text;
  vals: TInt64Array;

  Y_TARGET: Int64 = 2000000;      { Part 1 target row }
  MAX_COORD: Int64 = 4000000;     { Part 2 bounds }

  part1: Int64 = 0;
  intervals: array of TInterval;
  merged: array of TInterval;

  beaconsOnRow: array of TPoint;
  bcount: Integer;

  plusSet: array of Int64;
  minusSet: array of Int64;
  c1, c2: Int64;

  xCand, yCand: Int64;
  tuning: Int64;
  found: Boolean;

  dy, rem: Int64;
  l, rr: Int64;
  idx: Integer;

  tmp: TInterval;
  k: Integer;
  totalOnRow: Int64;

  sensorIdx: Integer;
  Btmp: TPoint;
  beaconExists: Boolean;

  maxLen: Integer;

begin
  { Read input from input.txt }
  SetLength(sensors, 0);
  N := 0;

  Assign(f, 'input.txt');
  Reset(f);
  while not Eof(f) do
  begin
    ReadLn(f, line);
    ExtractIntsFromLine(line, vals);
    if Length(vals) >= 4 then
    begin
      SetLength(sensors, N + 1);
      sensors[N].s.x := vals[0];
      sensors[N].s.y := vals[1];
      sensors[N].b.x := vals[2];
      sensors[N].b.y := vals[3];
      sensors[N].r := Abs64(vals[0] - vals[2]) + Abs64(vals[1] - vals[3]);
      Inc(N);
    end;
  end;
  Close(f);

  { Part 1: compute union of intervals on row Y_TARGET }
  SetLength(intervals, 0);
  for i := 0 to N - 1 do
  begin
    dy := Abs64(Y_TARGET - sensors[i].s.y);
    if dy <= sensors[i].r then
    begin
      rem := sensors[i].r - dy;
      l := sensors[i].s.x - rem;
      rr := sensors[i].s.x + rem;
      SetLength(intervals, Length(intervals) + 1);
      intervals[High(intervals)].l := l;
      intervals[High(intervals)].r := rr;
    end;
  end;

  { If there are intervals, sort and merge them }
  if Length(intervals) > 0 then
  begin
    SortIntervalsByL(intervals);
    SetLength(merged, 0);
    tmp.l := intervals[0].l;
    tmp.r := intervals[0].r;
    for i := 1 to Length(intervals) - 1 do
    begin
      if intervals[i].l > tmp.r + 1 then
      begin
        SetLength(merged, Length(merged) + 1);
        merged[High(merged)] := tmp;
        tmp.l := intervals[i].l;
        tmp.r := intervals[i].r;
      end
      else
      begin
        if intervals[i].r > tmp.r then tmp.r := intervals[i].r;
      end;
    end;
    SetLength(merged, Length(merged) + 1);
    merged[High(merged)] := tmp;

    { total covered positions on that row }
    part1 := 0;
    for i := 0 to High(merged) do
      part1 := part1 + (merged[i].r - merged[i].l + 1);

    { Collect beacons that lie on Y_TARGET (deduplicated) }
    SetLength(beaconsOnRow, 0);
    for i := 0 to N - 1 do
    begin
      if sensors[i].b.y = Y_TARGET then
      begin
        beaconExists := False;
        for j := 0 to High(beaconsOnRow) do
          if (beaconsOnRow[j].x = sensors[i].b.x) and (beaconsOnRow[j].y = sensors[i].b.y) then
          begin
            beaconExists := True;
            Break;
          end;
        if not beaconExists then
        begin
          SetLength(beaconsOnRow, Length(beaconsOnRow) + 1);
          beaconsOnRow[High(beaconsOnRow)] := sensors[i].b;
        end;
      end;
    end;

    { Subtract beacons that are within the union on that row }
    for i := 0 to High(beaconsOnRow) do
    begin
      if PointInUnion(beaconsOnRow[i].x, merged) then
        Dec(part1);
    end;
  end
  else
    part1 := 0;

  { Output Part 1 result (as first line) }
  WriteLn(part1);

  { Part 2: find the only possible distress beacon position }
  { Build plus/minus sets from each sensor's perimeter with distance r+1 }
  SetLength(plusSet, 0);
  SetLength(minusSet, 0);
  for i := 0 to N - 1 do
  begin
    rem := sensors[i].r + 1;
    { x+y = s.x + s.y +/- rem }
    SetLength(plusSet, Length(plusSet) + 1);
    plusSet[High(plusSet)] := sensors[i].s.x + sensors[i].s.y + rem;
    SetLength(plusSet, Length(plusSet) + 1);
    plusSet[High(plusSet)] := sensors[i].s.x + sensors[i].s.y - rem;

    { x-y = s.x - s.y +/- rem }
    SetLength(minusSet, Length(minusSet) + 1);
    minusSet[High(minusSet)] := sensors[i].s.x - sensors[i].s.y + rem;
    SetLength(minusSet, Length(minusSet) + 1);
    minusSet[High(minusSet)] := sensors[i].s.x - sensors[i].s.y - rem;
  end;

  tuning := 0;
  found := False;

  for i := 0 to High(plusSet) do
  begin
    for j := 0 to High(minusSet) do
    begin
      c1 := plusSet[i];
      c2 := minusSet[j];
      { require same parity for integer x,y }
      if (((c1 and 1) = (c2 and 1))) then
      begin
        xCand := (c1 + c2) div 2;
        yCand := (c1 - c2) div 2;
        if (xCand >= 0) and (xCand <= MAX_COORD) and (yCand >= 0) and (yCand <= MAX_COORD) then
        begin
          { Check if this point is outside all diamonds }
          found := True;
          for idx := 0 to N - 1 do
          begin
            if (Abs64(xCand - sensors[idx].s.x) + Abs64(yCand - sensors[idx].s.y)) <= sensors[idx].r then
            begin
              found := False;
              Break;
            end;
          end;
          if found then
          begin
            tuning := xCand * 4000000 + yCand;
            Break;
          end;
        end;
      end;
    end;
    if found then Break;
  end;

  WriteLn(tuning);
end.
