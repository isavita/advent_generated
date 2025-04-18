
program HandyHaversacks;
{$mode objfpc}{$H+}
uses
  SysUtils;

type
  TIntArray = array of Integer;
  TChild = record
    id, cnt: Integer;
  end;
  TChildArray = array of TChild;

var
  colors     : array of string;      // map index -> color
  rules      : array of TChildArray; // direct children for each color
  revAdj     : array of TIntArray;   // reverse graph: who can contain me
  dp         : array of LongInt;     // memo for part2
  f          : Text;

// get or assign a numeric id for a color
function getId(const c: string): Integer;
var
  i, n: Integer;
begin
  n := Length(colors);
  for i := 0 to n-1 do
    if colors[i] = c then
      Exit(i);
  // not found => add new
  Result := n;
  SetLength(colors,    n+1);
  SetLength(rules,     n+1);
  SetLength(revAdj,    n+1);
  colors[n] := c;
  // rules[n] and revAdj[n] default to empty
end;

// parse entire file
procedure ReadRules;
const
  SEP = ' bags contain ';
var
  line, rest, seg: string;
  p, commaPos, num, cid, parentID: Integer;
  cnt: Integer;
  parts: TChildArray;
begin
  AssignFile(f, 'input.txt');
  Reset(f);
  while not Eof(f) do
  begin
    ReadLn(f, line);
    if line = '' then Continue;
    // drop trailing period
    if line[Length(line)] = '.' then
      Delete(line, Length(line), 1);
    // container color
    p := Pos(SEP, line);
    parentID := getId(Copy(line, 1, p-1));
    rest := Copy(line, p + Length(SEP), MaxInt);
    if rest = 'no other bags' then
      Continue;
    // split by commas
    while rest <> '' do
    begin
      commaPos := Pos(',', rest);
      if commaPos > 0 then
      begin
        seg := Trim(Copy(rest, 1, commaPos-1));
        rest := Trim(Copy(rest, commaPos+1, MaxInt));
      end
      else
      begin
        seg := Trim(rest);
        rest := '';
      end;
      // seg = e.g. "2 muted yellow bags"
      p := Pos(' ', seg);
      num := StrToInt(Copy(seg, 1, p-1));
      // extract child color
      seg := Copy(seg, p+1, MaxInt);
      // remove trailing " bag" or " bags"
      seg := Copy(seg, 1, Pos(' bag', seg)-1);
      cid := getId(seg);
      // append to rules[parentID]
      cnt := Length(rules[parentID]);
      SetLength(rules[parentID], cnt+1);
      rules[parentID][cnt].id  := cid;
      rules[parentID][cnt].cnt := num;
    end;
  end;
  CloseFile(f);
end;

// build reverse adjacency after rules[] is filled
procedure BuildReverse;
var
  i, j, c: Integer;
begin
  for i := 0 to High(rules) do
    for j := 0 to High(rules[i]) do
    begin
      c := rules[i][j].id;
      // i can contain c  => in reverse, c -> i
      SetLength(revAdj[c], Length(revAdj[c]) + 1);
      revAdj[c][High(revAdj[c])] := i;
    end;
end;

// BFS from shinyGold over revAdj to count all reachable
function SolvePart1(shinyGold: Integer): Integer;
var
  visited: array of Boolean;
  queue: TIntArray;
  head, tail, u, v, i: Integer;
begin
  SetLength(visited, Length(colors));
  SetLength(queue,   Length(colors));
  head := 0; tail := 0;
  // start from shinyGold
  visited[shinyGold] := True;
  queue[tail] := shinyGold; Inc(tail);
  while head < tail do
  begin
    u := queue[head]; Inc(head);
    for i := 0 to High(revAdj[u]) do
    begin
      v := revAdj[u][i];
      if not visited[v] then
      begin
        visited[v] := True;
        queue[tail] := v; Inc(tail);
      end;
    end;
  end;
  // count all except shinyGold itself
  Result := 0;
  for i := 0 to High(visited) do
    if visited[i] and (i <> shinyGold) then
      Inc(Result);
end;

// recursive dp for how many bags a single bag i must contain
function CountInside(i: Integer): LongInt;
var
  sum: LongInt;
  k: Integer;
  ch: TChild;
begin
  if dp[i] >= 0 then
    Exit(dp[i]);
  sum := 0;
  for k := 0 to High(rules[i]) do
  begin
    ch := rules[i][k];
    // each of those ch.cnt bags contributes (1 + what's inside it)
    sum := sum + ch.cnt * (1 + CountInside(ch.id));
  end;
  dp[i] := sum;
  Result := sum;
end;

var
  shinyID: Integer;
  ans1: Integer;
  ans2: LongInt;
  n: Integer;
begin
  ReadRules;
  n := Length(colors);
  BuildReverse;
  // ensure 'shiny gold' exists and get its id
  shinyID := getId('shiny gold');

  // Part 1
  ans1 := SolvePart1(shinyID);

  // Part 2
  SetLength(dp, n);
  FillChar(dp[0], n * SizeOf(dp[0]), $FF); // fill with -1
  ans2 := CountInside(shinyID);

  // output
  WriteLn(ans1);
  WriteLn(ans2);
end.
