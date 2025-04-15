
program SolvePolygon;
type
  Coord = record
    x, y: Int64;
  end;

var
  vertices: array of Coord;
  n, i: Integer;

function AbsVal(x: Int64): Int64;
begin
  if x < 0 then AbsVal := -x else AbsVal := x;
end;

function Add(a, b: Coord): Coord;
begin
  Add.x := a.x + b.x;
  Add.y := a.y + b.y;
end;

function MulScalar(a: Coord; s: Int64): Coord;
begin
  MulScalar.x := a.x * s;
  MulScalar.y := a.y * s;
end;

function Shoelace(const arr: array of Coord): Int64;
var
  area: Int64;
  i, j, len: Integer;
begin
  area := 0;
  len := Length(arr);
  for i := 0 to len-1 do
  begin
    j := (i+1) mod len;
    area := area + arr[i].x * arr[j].y;
    area := area - arr[i].y * arr[j].x;
  end;
  Shoelace := AbsVal(area) div 2;
end;

function Perimeter(const arr: array of Coord): Int64;
var
  perim: Int64;
  i, j, len: Integer;
begin
  perim := 0;
  len := Length(arr);
  for i := 0 to len-1 do
  begin
    j := (i+1) mod len;
    perim := perim + AbsVal(arr[i].x - arr[j].x) + AbsVal(arr[i].y - arr[j].y);
  end;
  Perimeter := perim;
end;

function HexToInt(const s: string): Int64;
var
  res, i, v: Int64;
begin
  res := 0;
  for i := 1 to Length(s) do
  begin
    if s[i] in ['0'..'9'] then v := Ord(s[i]) - Ord('0')
    else v := 10 + Ord(UpCase(s[i])) - Ord('A');
    res := res * 16 + v;
  end;
  HexToInt := res;
end;

procedure ParseInput;
var
  f: Text;
  line: string;
  parts: array[1..3] of string;
  dir: char;
  len: Int64;
  i1, space1, space2: Integer;
  current, direction: Coord;
begin
  Assign(f, 'input.txt'); Reset(f);
  SetLength(vertices, 1);
  vertices[0].x := 0; vertices[0].y := 0;
  current.x := 0; current.y := 0;
  n := 1;
  while not EOF(f) do
  begin
    ReadLn(f, line);
    // Split line by space
    space1 := Pos(' ', line);
    space2 := Pos(' ', Copy(line, space1+1, Length(line)-space1)) + space1;
    parts[1] := Copy(line, 1, space1-1);
    parts[2] := Copy(line, space1+1, space2-space1-1);
    parts[3] := Copy(line, space2+1, Length(line)-space2);
    dir := parts[3][8];
    len := HexToInt(Copy(parts[3], 3, 5));
    case dir of
      '3': begin direction.x := 0;  direction.y := -1; end;
      '2': begin direction.x := -1; direction.y := 0; end;
      '1': begin direction.x := 0;  direction.y := 1;  end;
      '0': begin direction.x := 1;  direction.y := 0; end;
    end;
    current := Add(current, MulScalar(direction, len));
    Inc(n);
    SetLength(vertices, n);
    vertices[n-1] := current;
  end;
  Close(f);
end;

var
  area, peri: Int64;
begin
  ParseInput;
  area := Shoelace(vertices);
  peri := Perimeter(vertices);
  WriteLn(area + peri div 2 + 1);
end.
