
program WireIntersection;

{$mode objfpc}{$H+}

uses
  SysUtils;

const
  HASH_SIZE = 40000;

type
  PNode = ^TNode;
  TNode = record
    x, y   : Integer;
    wire   : Integer;   {1 for first wire, 2 for second}
    next   : PNode;
  end;

var
  table : array[0..HASH_SIZE-1] of PNode;

function HashKey(ax, ay: Integer): Integer;
var
  h: Int64;
begin
  h := (Int64(ax) * 31 + ay) mod HASH_SIZE;
  if h < 0 then h := h + HASH_SIZE;
  Result := Integer(h);
end;

procedure Insert(ax, ay, awire: Integer);
var
  idx : Integer;
  node: PNode;
begin
  idx := HashKey(ax, ay);
  New(node);
  node^.x := ax;
  node^.y := ay;
  node^.wire := awire;
  node^.next := table[idx];
  table[idx] := node;
end;

function Find(ax, ay: Integer): PNode;
var
  idx : Integer;
  cur : PNode;
begin
  idx := HashKey(ax, ay);
  cur := table[idx];
  while cur <> nil do
  begin
    if (cur^.x = ax) and (cur^.y = ay) then
      Exit(cur);
    cur := cur^.next;
  end;
  Result := nil;
end;

procedure ClearTable;
var
  i   : Integer;
  cur : PNode;
  nxt : PNode;
begin
  for i := 0 to HASH_SIZE-1 do
  begin
    cur := table[i];
    while cur <> nil do
    begin
      nxt := cur^.next;
      Dispose(cur);
      cur := nxt;
    end;
    table[i] := nil;
  end;
end;

procedure ProcessLine(const line: string; startWire: Integer; var minDist: Integer);
var
  rest   : string;
  token  : string;
  posCom : Integer;
  dir    : Char;
  steps  : Integer;
  i      : Integer;
  x, y   : Integer;
  node   : PNode;
begin
  rest := line;
  x := 0; y := 0;
  while rest <> '' do
  begin
    posCom := Pos(',', rest);
    if posCom = 0 then
    begin
      token := rest;
      rest := '';
    end
    else
    begin
      token := Copy(rest, 1, posCom-1);
      rest := Copy(rest, posCom+1, Length(rest));
    end;

    dir := token[1];
    steps := StrToInt(Copy(token, 2, Length(token)-1));

    for i := 1 to steps do
    begin
      case dir of
        'U': Inc(y);
        'D': Dec(y);
        'L': Dec(x);
        'R': Inc(x);
      end;

      if startWire = 2 then
      begin
        node := Find(x, y);
        if (node <> nil) and (node^.wire = 1) then
        begin
          if (Abs(x) + Abs(y) < minDist) then
            minDist := Abs(x) + Abs(y);
        end;
      end;

      Insert(x, y, startWire);
    end;
  end;
end;

var
  f        : TextFile;
  line1,
  line2    : string;
  minDist  : Integer;
begin
  AssignFile(f, 'input.txt');
  Reset(f);
  ReadLn(f, line1);
  ReadLn(f, line2);
  CloseFile(f);

  minDist := MaxInt;

  ProcessLine(Trim(line1), 1, minDist);   {first wire}
  ProcessLine(Trim(line2), 2, minDist);   {second wire, compute intersections}

  WriteLn(minDist);

  ClearTable;
end.
