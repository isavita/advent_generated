
program ElfMovement;

{$mode objfpc}{$H+}

const
  MAX_ELVES = 2000;
  MAP_SIZE = 1000;
  OFFSET = MAP_SIZE div 2;

type
  TPoint = record
    x, y: Integer;
  end;
  TElf = record
    pos, nextPos: TPoint;
    moving: Boolean;
  end;

var
  Dirs: array[0..7] of TPoint = (
    (x:-1; y:-1), (x:-1; y:0), (x:-1; y:1),
    (x:0;  y:1),  (x:1;  y:1), (x:1; y:0),
    (x:1;  y:-1), (x:0;  y:-1)
  );
  Order: array[0..3] of Integer = (1,5,7,3); {N,S,W,E as indices}
  currDir: Integer = 0;
  elves: array[0..MAX_ELVES-1] of TElf;
  numElves: Integer = 0;
  map: array[0..MAP_SIZE-1,0..MAP_SIZE-1] of Boolean;
  proposals: array[0..MAP_SIZE*MAP_SIZE-1] of Integer;

function HashPos(x, y: Integer): Integer;
begin
  Result := (x + OFFSET) * MAP_SIZE + (y + OFFSET);
end;

function IsOccupied(x, y: Integer): Boolean;
begin
  if (x < -OFFSET) or (x >= OFFSET) or (y < -OFFSET) or (y >= OFFSET) then
    Exit(False);
  Result := map[x + OFFSET, y + OFFSET];
end;

function AroundAllEmpty(e: TElf): Boolean;
var i: Integer;
begin
  for i := 0 to 7 do
    if IsOccupied(e.pos.x + Dirs[i].x, e.pos.y + Dirs[i].y) then
      Exit(False);
  Result := True;
end;

function ElfInDirection(e: TElf; dir: Integer): Boolean;
var j, d: Integer;
begin
  for j := -1 to 1 do
  begin
    d := (dir + j + 8) mod 8;
    if IsOccupied(e.pos.x + Dirs[d].x, e.pos.y + Dirs[d].y) then
      Exit(True);
  end;
  Result := False;
end;

function RunRound: Boolean;
var i, j, dir, h: Integer;
    e: ^TElf;
begin
  FillChar(proposals, SizeOf(proposals), 0);
  Result := False;
  for i := 0 to numElves - 1 do
  begin
    e := @elves[i];
    e^.moving := False;
    if AroundAllEmpty(e^) then Continue;
    for j := 0 to 3 do
    begin
      dir := Order[(currDir + j) mod 4];
      if ElfInDirection(e^, dir) then Continue;
      e^.nextPos.x := e^.pos.x + Dirs[dir].x;
      e^.nextPos.y := e^.pos.y + Dirs[dir].y;
      h := HashPos(e^.nextPos.x, e^.nextPos.y);
      Inc(proposals[h]);
      e^.moving := True;
      Break;
    end;
  end;
  for i := 0 to numElves - 1 do
  begin
    e := @elves[i];
    if not e^.moving then Continue;
    h := HashPos(e^.nextPos.x, e^.nextPos.y);
    if proposals[h] > 1 then Continue;
    Result := True;
    map[e^.pos.x + OFFSET, e^.pos.y + OFFSET] := False;
    map[e^.nextPos.x + OFFSET, e^.nextPos.y + OFFSET] := True;
    e^.pos := e^.nextPos;
    e^.moving := False;
  end;
  currDir := (currDir + 1) mod 4;
end;

var
  f: TextFile;
  line: string;
  row, col: Integer;
begin
  AssignFile(f, 'input.txt');
  Reset(f);
  row := 0;
  while not Eof(f) do
  begin
    ReadLn(f, line);
    for col := 1 to Length(line) do
      if line[col] = '#' then
      begin
        elves[numElves].pos.x := row;
        elves[numElves].pos.y := col - 1;
        elves[numElves].moving := False;
        Inc(numElves);
        map[row + OFFSET, (col - 1) + OFFSET] := True;
      end;
    Inc(row);
  end;
  CloseFile(f);
  row := 0;
  while RunRound do Inc(row);
  WriteLn(row + 1);
end.
