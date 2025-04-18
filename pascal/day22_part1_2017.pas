
{$mode objfpc}{$H+}
program SporificaVirus;

uses
  SysUtils, Classes;

type
  PNode = ^TNode;
  TNode = record
    x, y: Integer;
    next: PNode;
  end;

const
  // A prime bucket count for our hash table
  BUCKET_COUNT = 20011;

var
  buckets: array[0..BUCKET_COUNT-1] of PNode;

// Simple hash function for (x,y)
function HashXY(x, y: Integer): Integer;
var
  h: Int64;
begin
  // Combine x,y into one 64-bit and take mod BUCKET_COUNT
  h := (Int64(x) * 1000003) xor y;
  h := h mod BUCKET_COUNT;
  if h < 0 then
    h := h + BUCKET_COUNT;
  Result := h;
end;

// Returns True if (x,y) was in the set
function IsInfected(x, y: Integer): Boolean;
var
  b: Integer;
  p: PNode;
begin
  b := HashXY(x, y);
  p := buckets[b];
  while Assigned(p) do
  begin
    if (p^.x = x) and (p^.y = y) then
    begin
      Result := True;
      Exit;
    end;
    p := p^.next;
  end;
  Result := False;
end;

// Toggles the state at (x,y):
//   if it was infected, remove it and return False;
//   if it was clean, add it and return True.
function ToggleInfection(x, y: Integer): Boolean;
var
  b: Integer;
  p, prev, nn: PNode;
begin
  b := HashXY(x, y);
  p := buckets[b];
  prev := nil;
  while Assigned(p) do
  begin
    if (p^.x = x) and (p^.y = y) then
    begin
      // found: remove it
      if prev = nil then
        buckets[b] := p^.next
      else
        prev^.next := p^.next;
      Dispose(p);
      ToggleInfection := False;
      Exit;
    end;
    prev := p;
    p := p^.next;
  end;
  // not found: add new node
  New(nn);
  nn^.x := x;
  nn^.y := y;
  nn^.next := buckets[b];
  buckets[b] := nn;
  ToggleInfection := True;
end;

var
  input  : TStringList;
  row, col, center, bursts, infections: Integer;
  dir, x, y: Integer;
  line: string;
// Directions: 0=up,1=right,2=down,3=left
const
  dx: array[0..3] of Integer = (0, 1, 0, -1);
  dy: array[0..3] of Integer = (-1, 0, 1, 0);

begin
  // Read the input grid
  input := TStringList.Create;
  try
    input.LoadFromFile('input.txt');
    row := input.Count;
    if row = 0 then
    begin
      Writeln('Error: input.txt is empty');
      Exit;
    end;
    col := Length(input[0]);
    // The virus starts in the middle of the map
    center := row div 2;
    // Initialize the hash table
    FillChar(buckets, SizeOf(buckets), 0);
    // Insert initial infections
    for y := 0 to row-1 do
      for x := 1 to col do
        if input[y][x] = '#' then
          // translate map coords to centered coords
          ToggleInfection(x-1 - center, y - center)
        else
          ; // clean nodes we leave out
    // Initialize carrier
    x := 0; y := 0; dir := 0; 
    bursts := 10000;
    infections := 0;
    // Simulate bursts
    while bursts > 0 do
    begin
      // 1) turn
      if IsInfected(x, y) then
        dir := (dir + 1) and 3  // turn right
      else
        dir := (dir + 3) and 3; // turn left
      // 2) toggle infection, count if it became infected
      if ToggleInfection(x, y) then
        Inc(infections);
      // 3) move
      Inc(x, dx[dir]);
      Inc(y, dy[dir]);
      Dec(bursts);
    end;
    // Output result
    Writeln(infections);
  finally
    input.Free;
    // Note: we do not free all linked nodes here.  Program is ending.
  end;
end.
