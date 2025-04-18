
program MarbleMania;
{$mode objfpc}
uses
  SysUtils;

var
  players, lastMarble: Integer;
  scores: array of Int64;
  nxt, prv: array of Integer;
  current, m, player, i: Integer;
  line: string;
  nums: array[0..1] of Integer;
  numCount, num, idx: Integer;
  ch: Char;
  maxScore: Int64;

begin
  { Read and parse input }
  Assign(Input,'input.txt'); Reset(Input);
  if EOF then
  begin
    Writeln('No input');
    Halt(1);
  end;
  Readln(line);
  Close(Input);

  { Extract two integers from the line }
  numCount := 0;
  num := 0;
  for ch in line do
  begin
    if ch in ['0'..'9'] then
    begin
      num := num * 10 + Ord(ch) - Ord('0');
    end
    else
    begin
      if (num <> 0) or (ch='0') then
      begin
        if numCount < 2 then
        begin
          nums[numCount] := num;
          Inc(numCount);
        end;
        num := 0;
      end;
    end;
    if numCount = 2 then Break;
  end;
  if numCount < 2 then
  begin
    Writeln('Failed to parse input line.');
    Halt(1);
  end;
  players := nums[0];
  lastMarble := nums[1];

  { Prepare arrays: we will index marbles 0..lastMarble }
  SetLength(nxt, lastMarble+1);
  SetLength(prv, lastMarble+1);
  SetLength(scores, players);

  { Initially, marble 0 points to itself }
  nxt[0] := 0;
  prv[0] := 0;
  current := 0;

  { Simulate }
  for m := 1 to lastMarble do
  begin
    player := (m-1) mod players;
    if (m mod 23) <> 0 then
    begin
      { Standard insertion: go one step clockwise, then insert }
      current := nxt[current];
      idx := nxt[current];
      { link m between current and idx }
      nxt[current] := m;
      prv[m] := current;
      nxt[m] := idx;
      prv[idx] := m;
      current := m;
    end
    else
    begin
      { Scoring turn }
      scores[player] := scores[player] + m;
      { move 7 steps counter-clockwise }
      for i := 1 to 7 do
        current := prv[current];
      { remove 'current' }
      scores[player] := scores[player] + current;
      idx := nxt[current];
      nxt[prv[current]] := idx;
      prv[idx] := prv[current];
      current := idx;
    end;
  end;

  { Find and print maximum score }
  maxScore := 0;
  for i := 0 to players-1 do
    if scores[i] > maxScore then
      maxScore := scores[i];

  Writeln(maxScore);
end.
