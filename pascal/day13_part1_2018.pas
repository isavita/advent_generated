program MineCartMadness;
{ Reads a track layout from input.txt, simulates carts until the first collision,
  and prints the collision coordinates as “X,Y” to standard output. }

type
  TDir = (Up, Right, Down, Left);
  TCart = record
    x, y: Integer;
    dir: TDir;
    nextTurn: Integer;  { 0 = left, 1 = straight, 2 = right }
  end;

const
  MAXCARTS = 1000;

var
  lines: array of string;
  grid: array of array of Char;
  carts: array[1..MAXCARTS] of TCart;
  cartCount: Integer;
  width, height: Integer;

{ Reads all lines from input.txt into the dynamic array lines,
  determines width=longest line, pads shorter lines with spaces. }
procedure ReadInput;
var
  f: Text;
  s: string;
  maxLen, i: Integer;
begin
  Assign(f, 'input.txt');
  Reset(f);
  maxLen := 0; height := 0;
  while not Eof(f) do
  begin
    ReadLn(f, s);
    Inc(height);
    if Length(s) > maxLen then
      maxLen := Length(s);
  end;
  Close(f);

  width := maxLen;
  SetLength(lines, height);

  Assign(f, 'input.txt');
  Reset(f);
  i := 0;
  while not Eof(f) do
  begin
    ReadLn(f, s);
    while Length(s) < width do
      s := s + ' ';
    lines[i] := s;
    Inc(i);
  end;
  Close(f);
end;

{ Parses lines into grid and extracts carts, replacing cart symbols
  with the underlying track piece. }
procedure Parse;
var
  x, y: Integer;
  c: Char;
begin
  SetLength(grid, height);
  cartCount := 0;
  for y := 0 to height - 1 do
  begin
    SetLength(grid[y], width);
    for x := 0 to width - 1 do
    begin
      c := lines[y][x + 1];
      case c of
        '^', 'v', '<', '>':
          begin
            Inc(cartCount);
            carts[cartCount].x := x;
            carts[cartCount].y := y;
            carts[cartCount].nextTurn := 0;
            case c of
              '^': carts[cartCount].dir := Up;
              'v': carts[cartCount].dir := Down;
              '<': carts[cartCount].dir := Left;
              '>': carts[cartCount].dir := Right;
            end;
            if (c = '^') or (c = 'v') then
              grid[y][x] := '|'
            else
              grid[y][x] := '-';
          end;
      else
        grid[y][x] := c;
      end;
    end;
  end;
end;

{ Simple swap for two carts }
procedure SwapCarts(var A, B: TCart);
var
  tmp: TCart;
begin
  tmp := A;
  A := B;
  B := tmp;
end;

{ Sort carts by Y then X (top-to-bottom, left-to-right) }
procedure SortCarts;
var
  i, j: Integer;
begin
  for i := 1 to cartCount - 1 do
    for j := i + 1 to cartCount do
      if (carts[i].y > carts[j].y) or
         ((carts[i].y = carts[j].y) and (carts[i].x > carts[j].x)) then
        SwapCarts(carts[i], carts[j]);
end;

{ Runs the simulation until the first collision is found.
  Prints “X,Y” and halts immediately. }
procedure Simulate;
var
  i, j, cx, cy: Integer;
  cell: Char;
begin
  while True do
  begin
    SortCarts;
    for i := 1 to cartCount do
    begin
      { Move one step }
      case carts[i].dir of
        Up:    Dec(carts[i].y);
        Down:  Inc(carts[i].y);
        Left:  Dec(carts[i].x);
        Right: Inc(carts[i].x);
      end;
      cx := carts[i].x; 
      cy := carts[i].y;
      { Check for collisions }
      for j := 1 to cartCount do
        if (j <> i) and
           (carts[j].x = cx) and
           (carts[j].y = cy) then
        begin
          WriteLn(cx, ',', cy);
          Halt;
        end;
      { Adjust direction based on track piece }
      cell := grid[cy][cx];
      case cell of
        '/': case carts[i].dir of
               Up:    carts[i].dir := Right;
               Right: carts[i].dir := Up;
               Down:  carts[i].dir := Left;
               Left:  carts[i].dir := Down;
             end;
        '\': case carts[i].dir of
               Up:    carts[i].dir := Left;
               Left:  carts[i].dir := Up;
               Down:  carts[i].dir := Right;
               Right: carts[i].dir := Down;
             end;
        '+': begin
               { intersection: left, straight, right in cycle }
               case carts[i].nextTurn of
                 0: carts[i].dir := TDir((Ord(carts[i].dir) + 3) mod 4);
                 2: carts[i].dir := TDir((Ord(carts[i].dir) + 1) mod 4);
               end;
               carts[i].nextTurn := (carts[i].nextTurn + 1) mod 3;
             end;
      end;
    end;
  end;
end;

begin
  ReadInput;
  Parse;
  Simulate;
end.
