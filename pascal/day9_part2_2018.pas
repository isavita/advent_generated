
program MarbleMania;
{$mode objfpc}{$H+}
uses
  SysUtils;

{ Play the marble game for numPlayers and lastMarble; return the winning score. }
function Play(numPlayers, lastMarble: Integer): Int64;
var
  Next, Prev: array of Integer;
  Scores: array of Int64;
  current, marble, player, rem, p, n, i: Integer;
begin
  SetLength(Next, lastMarble + 1);
  SetLength(Prev, lastMarble + 1);
  SetLength(Scores, numPlayers);
  { initialize circle with marble 0 }
  Next[0] := 0; Prev[0] := 0;
  current := 0;
  { play }
  for marble := 1 to lastMarble do
  begin
    player := (marble - 1) mod numPlayers;
    if (marble mod 23) <> 0 then
    begin
      { insert new marble between current.Next and that.Next }
      p := Next[current];
      n := Next[p];
      Next[p] := marble;
      Prev[marble] := p;
      Next[marble] := n;
      Prev[n] := marble;
      current := marble;
    end
    else
    begin
      { scoring move }
      Scores[player] := Scores[player] + marble;
      rem := current;
      { move 7 counter‐clockwise }
      for i := 1 to 7 do
        rem := Prev[rem];
      Scores[player] := Scores[player] + rem;
      { remove rem from circle }
      p := Prev[rem];
      n := Next[rem];
      Next[p] := n;
      Prev[n] := p;
      current := n;
    end;
  end;
  { find best score }
  Result := 0;
  for player := 0 to numPlayers - 1 do
    if Scores[player] > Result then
      Result := Scores[player];
end;

var
  f: TextFile;
  line: string;
  nums: array[1..2] of Integer;
  i, cnt, tmp: Integer;
  ch: Char;
  nPlayers, lastMarble: Integer;
  part1, part2: Int64;
begin
  { read the single input line }
  AssignFile(f, 'input.txt');
  Reset(f);
  if not Eof(f) then
    ReadLn(f, line);
  CloseFile(f);

  { extract the two integers from the line }
  cnt := 0; tmp := 0;
  for i := 1 to Length(line) do
  begin
    ch := line[i];
    if ch in ['0'..'9'] then
      tmp := tmp * 10 + (Ord(ch) - Ord('0'))
    else if tmp > 0 then
    begin
      Inc(cnt);
      if cnt <= 2 then
        nums[cnt] := tmp;
      tmp := 0;
    end;
  end;
  if (tmp > 0) and (cnt < 2) then
  begin
    Inc(cnt);
    nums[cnt] := tmp;
  end;
  if cnt < 2 then
  begin
    Writeln('Error: could not parse input.');
    Halt(1);
  end;

  nPlayers := nums[1];
  lastMarble := nums[2];

  { part 1: normal game }
  part1 := Play(nPlayers, lastMarble);
  { part 2: 100× last marble }
  part2 := Play(nPlayers, lastMarble * 100);

  Writeln(part1);
  Writeln(part2);
end.
