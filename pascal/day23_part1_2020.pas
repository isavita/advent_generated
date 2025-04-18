Program CrabCups;
{ Reads a single line of digits from input.txt, simulates 100 moves of the
  Crab Cups game, and writes the labels after cup 1 as one concatenated string. }
const
  MAXN = 10000;
var
  inputLine: string;
  nextCup: array[1..MAXN] of integer;
  n, i, current, pick1, pick2, pick3, dest, minLabel, maxLabel: integer;
  p: integer;
  resultStr: string;
begin
  { Read the input }
  Assign(Input, 'input.txt');
  Reset(Input);
  ReadLn(inputLine);
  Close(Input);

  { Number of cups }
  n := Length(inputLine);

  { Determine the minimum and maximum labels (for wrapping) }
  minLabel := MaxInt;
  maxLabel := -MaxInt;
  for i := 1 to n do
  begin
    current := Ord(inputLine[i]) - Ord('0');
    if current < minLabel then
      minLabel := current;
    if current > maxLabel then
      maxLabel := current;
  end;

  { Build the circular linked list in the array `nextCup` }
  for i := 1 to n - 1 do
    nextCup[Ord(inputLine[i]) - Ord('0')] := Ord(inputLine[i + 1]) - Ord('0');
  nextCup[Ord(inputLine[n]) - Ord('0')] := Ord(inputLine[1]) - Ord('0');

  { Start with the first cup as current }
  current := Ord(inputLine[1]) - Ord('0');

  { Perform 100 moves }
  for i := 1 to 100 do
  begin
    { Pick up three cups immediately clockwise of current }
    pick1 := nextCup[current];
    pick2 := nextCup[pick1];
    pick3 := nextCup[pick2];

    { Remove them from the circle }
    nextCup[current] := nextCup[pick3];

    { Select destination label }
    dest := current - 1;
    if dest < minLabel then
      dest := maxLabel;
    while (dest = pick1) or (dest = pick2) or (dest = pick3) do
    begin
      Dec(dest);
      if dest < minLabel then
        dest := maxLabel;
    end;

    { Insert the picked-up cups after the destination }
    nextCup[pick3] := nextCup[dest];
    nextCup[dest] := pick1;

    { New current cup is the one immediately clockwise of the old current }
    current := nextCup[current];
  end;

  { Collect the labels after cup 1 }
  resultStr := '';
  p := nextCup[1];
  while p <> 1 do
  begin
    resultStr := resultStr + Chr(Ord('0') + p);
    p := nextCup[p];
  end;

  { Output the result }
  WriteLn(resultStr);
end.