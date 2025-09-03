program DoorsPathPas;
{$mode objfpc}{$H+}
uses
  SysUtils;

type
  TPoint = record
    X, Y: Integer;
  end;

  PMapNode = ^MapNode;
  MapNode = record
    Key: TPoint;
    Value: Integer;
    Next: PMapNode;
  end;

  PMap = ^TMap;
  TMap = record
    Table: array of PMapNode;
    Capacity: Integer;
    Size: Integer;
  end;

  TStack = record
    Items: array of TPoint;
    Top: Integer;
  end;

function HashPoint(p: TPoint; capacity: Integer): Integer;
var
  h: Int64;
begin
  h := Int64(p.X) * 31 + Int64(p.Y);
  h := h mod capacity;
  if h < 0 then h := h + capacity;
  Result := Integer(h);
end;

function MapCreate(initial_capacity: Integer): PMap;
var
  m: PMap;
  i: Integer;
begin
  New(m);
  m^.Capacity := initial_capacity;
  m^.Size := 0;
  SetLength(m^.Table, initial_capacity);
  for i := 0 to initial_capacity - 1 do
    m^.Table[i] := nil;
  Result := m;
end;

procedure MapDestroy(var m: PMap);
var
  i: Integer;
  cur, nxt: PMapNode;
begin
  if m = nil then Exit;
  for i := 0 to m^.Capacity - 1 do
  begin
    cur := m^.Table[i];
    while cur <> nil do
    begin
      nxt := cur^.Next;
      Dispose(cur);
      cur := nxt;
    end;
  end;
  SetLength(m^.Table, 0);
  Dispose(m);
  m := nil;
end;

function MapGet(m: PMap; key: TPoint): Integer;
var
  idx: Integer;
  cur: PMapNode;
begin
  Result := -1;
  if m = nil then Exit;
  idx := HashPoint(key, m^.Capacity);
  cur := m^.Table[idx];
  while cur <> nil do
  begin
    if (cur^.Key.X = key.X) and (cur^.Key.Y = key.Y) then
    begin
      Result := cur^.Value;
      Exit;
    end;
    cur := cur^.Next;
  end;
end;

function MapUpdateMin(m: PMap; key: TPoint; value: Integer): Boolean;
var
  idx: Integer;
  cur: PMapNode;
  nd: PMapNode;
begin
  Result := False;
  if m = nil then Exit;
  idx := HashPoint(key, m^.Capacity);
  cur := m^.Table[idx];
  while cur <> nil do
  begin
    if (cur^.Key.X = key.X) and (cur^.Key.Y = key.Y) then
    begin
      if value < cur^.Value then
      begin
        cur^.Value := value;
        Result := True;
      end;
      Exit;
    end;
    cur := cur^.Next;
  end;

  New(nd);
  nd^.Key := key;
  nd^.Value := value;
  nd^.Next := m^.Table[idx];
  m^.Table[idx] := nd;
  Inc(m^.Size);
  Result := True;
end;

procedure StackInit(var st: TStack; initial_capacity: Integer);
begin
  SetLength(st.Items, initial_capacity);
  st.Top := 0;
end;

procedure StackPush(var st: TStack; item: TPoint);
begin
  if st.Top = Length(st.Items) then
    SetLength(st.Items, Length(st.Items) * 2);
  st.Items[st.Top] := item;
  Inc(st.Top);
end;

function StackPeek(var st: TStack): TPoint;
begin
  Result := st.Items[st.Top - 1];
end;

function StackPop(var st: TStack): TPoint;
begin
  Dec(st.Top);
  Result := st.Items[st.Top];
end;

var
  f: Text;
  input: string;
  line: string;
  startPos, endPos, i: Integer;
  map: PMap;
  st: TStack;
  current, nextPos: TPoint;
  curDoors, nextDoors: Integer;
  maxDoors, rooms1000: Integer;
  node: PMapNode;
  idx: Integer;
begin
  AssignFile(f, 'input.txt');
  Reset(f);
  input := '';
  while not Eof(f) do
  begin
    ReadLn(f, line);
    input := input + line;
  end;
  CloseFile(f);

  startPos := Pos('^', input);
  if startPos > 0 then Inc(startPos);
  endPos := Pos('$', input);
  if endPos = 0 then endPos := Length(input) + 1;

  map := MapCreate(16384);
  StackInit(st, 1024);

  current.X := 0; current.Y := 0;
  MapUpdateMin(map, current, 0);

  if startPos <= Length(input) then
  begin
    for i := startPos to endPos - 1 do
    begin
      case input[i] of
        '(':
          StackPush(st, current);
        '|':
          current := StackPeek(st);
        ')':
          current := StackPop(st);
        'N':
        begin
          nextPos.X := current.X;
          nextPos.Y := current.Y - 1;
          curDoors := MapGet(map, current);
          nextDoors := curDoors + 1;
          MapUpdateMin(map, nextPos, nextDoors);
          current := nextPos;
        end;
        'E':
        begin
          nextPos.X := current.X + 1;
          nextPos.Y := current.Y;
          curDoors := MapGet(map, current);
          nextDoors := curDoors + 1;
          MapUpdateMin(map, nextPos, nextDoors);
          current := nextPos;
        end;
        'S':
        begin
          nextPos.X := current.X;
          nextPos.Y := current.Y + 1;
          curDoors := MapGet(map, current);
          nextDoors := curDoors + 1;
          MapUpdateMin(map, nextPos, nextDoors);
          current := nextPos;
        end;
        'W':
        begin
          nextPos.X := current.X - 1;
          nextPos.Y := current.Y;
          curDoors := MapGet(map, current);
          nextDoors := curDoors + 1;
          MapUpdateMin(map, nextPos, nextDoors);
          current := nextPos;
        end;
      end;
    end;
  end;

  maxDoors := 0;
  rooms1000 := 0;
  for idx := 0 to map^.Capacity - 1 do
  begin
    node := map^.Table[idx];
    while node <> nil do
    begin
      if node^.Value > maxDoors then maxDoors := node^.Value;
      if node^.Value >= 1000 then Inc(rooms1000);
      node := node^.Next;
    end;
  end;

  WriteLn(maxDoors);
  WriteLn(rooms1000);

  MapDestroy(map);
end.