
program Solution;

type
  TCoord = record
    x, y, z: Integer;
  end;

  TBrick = record
    mini, maxi: TCoord;
    basedOn: array of Integer;
    support: array of Integer;
  end;

var
  bricks: array of TBrick;
  n: Integer;

function min(a, b: Integer): Integer;
begin
  if a < b then
    min := a
  else
    min := b;
end;

function max(a, b: Integer): Integer;
begin
  if a > b then
    max := a
  else
    max := b;
end;

procedure parseInput(filename: string);
var
  f: Text;
  s: string;
  i: Integer;
begin
  Assign(f, filename);
  Reset(f);
  n := 0;
  while not Eof(f) do
  begin
    ReadLn(f, s);
    Inc(n);
  end;
  SetLength(bricks, n);
  Reset(f);
  for i := 0 to n - 1 do
  begin
    ReadLn(f, s);
    SetLength(bricks[i].basedOn, 0);
    SetLength(bricks[i].support, 0);
    Val(Copy(s, 1, Pos(',', s) - 1), bricks[i].mini.x);
    Delete(s, 1, Pos(',', s));
    Val(Copy(s, 1, Pos(',', s) - 1), bricks[i].mini.y);
    Delete(s, 1, Pos(',', s));
    Val(Copy(s, 1, Pos('~', s) - 1), bricks[i].mini.z);
    Delete(s, 1, Pos('~', s));
    Val(Copy(s, 1, Pos(',', s) - 1), bricks[i].maxi.x);
    Delete(s, 1, Pos(',', s));
    Val(Copy(s, 1, Pos(',', s) - 1), bricks[i].maxi.y);
    Delete(s, 1, Pos(',', s));
    Val(s, bricks[i].maxi.z);
  end;
  Close(f);
end;

procedure settle();
var
  i, j, supportZ, deltaZ: Integer;
  isIntersectingX, isIntersectingY, isIntersecting: Boolean;
  temp: TBrick;
begin
  for i := 0 to n - 2 do
    for j := 0 to n - i - 2 do
      if bricks[j].maxi.z > bricks[j + 1].maxi.z then
      begin
        temp := bricks[j];
        bricks[j] := bricks[j + 1];
        bricks[j + 1] := temp;
      end;

  for i := 0 to n - 1 do
  begin
    supportZ := 0;
    SetLength(bricks[i].basedOn, 0);
    for j := i - 1 downto 0 do
    begin
      isIntersectingX := max(bricks[i].mini.x, bricks[j].mini.x) <= min(bricks[i].maxi.x, bricks[j].maxi.x);
      isIntersectingY := max(bricks[i].mini.y, bricks[j].mini.y) <= min(bricks[i].maxi.y, bricks[j].maxi.y);
      isIntersecting := isIntersectingX and isIntersectingY;
      if isIntersecting then
      begin
        if bricks[j].maxi.z = supportZ then
        begin
          SetLength(bricks[i].basedOn, Length(bricks[i].basedOn) + 1);
          bricks[i].basedOn[High(bricks[i].basedOn)] := j;
        end
        else if bricks[j].maxi.z > supportZ then
        begin
          supportZ := bricks[j].maxi.z;
          SetLength(bricks[i].basedOn, 1);
          bricks[i].basedOn[0] := j;
        end;
      end;
    end;

    for j := 0 to High(bricks[i].basedOn) do
    begin
      SetLength(bricks[bricks[i].basedOn[j]].support, Length(bricks[bricks[i].basedOn[j]].support) + 1);
      bricks[bricks[i].basedOn[j]].support[High(bricks[bricks[i].basedOn[j]].support)] := i;
    end;

    deltaZ := bricks[i].maxi.z - bricks[i].mini.z;
    bricks[i].mini.z := supportZ + 1;
    bricks[i].maxi.z := bricks[i].mini.z + deltaZ;
  end;
end;

function solve(): Integer;
var
  i, j, cnt: Integer;
  isDisintegratable: Boolean;
begin
  settle();
  cnt := 0;
  for i := 0 to n - 1 do
  begin
    isDisintegratable := True;
    for j := 0 to High(bricks[i].support) do
    begin
      if Length(bricks[bricks[i].support[j]].basedOn) < 2 then
      begin
        isDisintegratable := False;
        Break;
      end;
    end;
    if isDisintegratable then
      Inc(cnt);
  end;
  solve := cnt;
end;

begin
  parseInput('input.txt');
  WriteLn(solve());
end.
