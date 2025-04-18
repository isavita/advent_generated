
program KnightsOfTheDinnerTable;
{$mode objfpc}{$h+}
uses
  SysUtils;

const
  MAXP = 20;

var
  fin: Text;
  line: string;
  names: array[1..MAXP] of string;
  nNames: Integer = 0;
  happy: array[1..MAXP,1..MAXP] of Integer;

  // For permutation
  arr: array[1..MAXP] of Integer;
  best1, best2: Integer;

function GetIndex(const nm: string): Integer;
var
  i: Integer;
begin
  for i := 1 to nNames do
    if names[i] = nm then
      Exit(i);
  Inc(nNames);
  names[nNames] := nm;
  Result := nNames;
end;

// Split s by spaces into parts[0..] and return count
function SplitString(const s: string; var parts: array of string): Integer;
var
  i, j, L: Integer;
begin
  i := 1; L := Length(s); Result := 0;
  while i <= L do
  begin
    while (i <= L) and (s[i] = ' ') do Inc(i);
    if i > L then Break;
    j := i;
    while (j <= L) and (s[j] <> ' ') do Inc(j);
    parts[Result] := Copy(s, i, j - i);
    Inc(Result);
    i := j + 1;
  end;
end;

// Evaluate current arr[1..nP] as a circle
function EvalCircle(nP: Integer): Integer;
var
  i, j, sum: Integer;
begin
  sum := 0;
  for i := 1 to nP do
  begin
    j := i mod nP + 1;
    sum += happy[arr[i], arr[j]] + happy[arr[j], arr[i]];
  end;
  Result := sum;
end;

// Generate permutations of arr[2..nP], arr[1] fixed = 1
procedure Permute(pos, nP: Integer; var best: Integer);
var
  i, tmp, val: Integer;
begin
  if pos > nP then
  begin
    val := EvalCircle(nP);
    if val > best then best := val;
    Exit;
  end;
  for i := pos to nP do
  begin
    tmp := arr[pos]; arr[pos] := arr[i]; arr[i] := tmp;
    Permute(pos + 1, nP, best);
    // swap back
    tmp := arr[pos]; arr[pos] := arr[i]; arr[i] := tmp;
  end;
end;

var
  parts: array[0..15] of string;
  cnt, i, p1, p2, amt: Integer;
  action: string;
begin
  // Initialize
  for i := 1 to MAXP do
    for p1 := 1 to MAXP do
      happy[i,p1] := 0;

  // Open input.txt
  AssignFile(fin, 'input.txt');
  {$I-} Reset(fin); {$I+}
  if IOResult <> 0 then
  begin
    WriteLn('Cannot open input.txt');
    Halt(1);
  end;

  // Read and parse
  while not EOF(fin) do
  begin
    ReadLn(fin, line);
    if line = '' then Continue;
    cnt := SplitString(line, parts);
    // parts[0]=Name1, [2]=gain/lose, [3]=number, [10]=Name2.
    action := parts[2];
    amt := StrToInt(parts[3]);
    if action = 'lose' then amt := -amt;
    // strip trailing period from parts[10]
    p2 := Length(parts[10]);
    if parts[10][p2] = '.' then
      parts[10] := Copy(parts[10], 1, p2 - 1);
    p1 := GetIndex(parts[0]);
    p2 := GetIndex(parts[10]);
    happy[p1, p2] := amt;
  end;
  CloseFile(fin);

  // Part 1: without "You"
  // Prepare arr[1..nNames], fix arr[1]=1
  for i := 1 to nNames do
    arr[i] := i;
  best1 := -MaxInt;
  Permute(2, nNames, best1);

  // Part 2: add "You" as a new guest with zero edges
  Inc(nNames);
  names[nNames] := 'You';
  // happy[*][nNames] and happy[nNames][*] are already zero
  for i := 1 to nNames do
    arr[i] := i;
  best2 := -MaxInt;
  Permute(2, nNames, best2);

  // Output results
  WriteLn(best1);
  WriteLn(best2);
end.
