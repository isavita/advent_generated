
program DiracDice;
{$mode objfpc}{$H+}
uses
  SysUtils;

type
  TInt64 = Int64;
  TPair = record
    w1, w2: TInt64;
  end;

const
  TARGET1 = 1000;
  TARGET2 = 21;
  DICE_SIDES = 100;

var
  // Memoization for part 2: dimensions are:
  // pos1 = 1..10, pos2 = 1..10, score1 = 0..21, score2 = 0..21, player = 0..1
  memoVis: array[1..10,1..10,0..TARGET2,0..TARGET2,0..1] of Boolean;
  memoVal: array[1..10,1..10,0..TARGET2,0..TARGET2,0..1] of TPair;
  freq: array[3..9] of Integer;

// DiracWins returns (wins1, wins2) from the given state for part two.
function DiracWins(p1, p2, s1, s2, player: Integer): TPair;
var
  sum, c, np, ns: Integer;
  res, sub: TPair;
begin
  // If either already reached target, immediate result
  if s1 >= TARGET2 then
  begin
    res.w1 := 1; res.w2 := 0;
    Exit(res);
  end;
  if s2 >= TARGET2 then
  begin
    res.w1 := 0; res.w2 := 1;
    Exit(res);
  end;

  // Check memo
  if memoVis[p1,p2,s1,s2,player] then
  begin
    Exit(memoVal[p1,p2,s1,s2,player]);
  end;

  res.w1 := 0;
  res.w2 := 0;
  // For each possible sum of three 3‐sided dice
  for sum := 3 to 9 do
  begin
    c := freq[sum];
    if c = 0 then Continue;
    if player = 0 then
    begin
      // Player 1's turn
      np := ((p1 - 1 + sum) mod 10) + 1;
      ns := s1 + np;
      sub := DiracWins(np, p2, ns, s2, 1);
    end
    else
    begin
      // Player 2's turn
      np := ((p2 - 1 + sum) mod 10) + 1;
      ns := s2 + np;
      sub := DiracWins(p1, np, s1, ns, 0);
    end;
    // Accumulate, scaling by how many universes this sum has
    res.w1 := res.w1 + sub.w1 * c;
    res.w2 := res.w2 + sub.w2 * c;
  end;

  memoVis[p1,p2,s1,s2,player] := True;
  memoVal[p1,p2,s1,s2,player] := res;
  Exit(res);
end;

var
  f : TextFile;
  line: string;
  p1, p2: Integer;
  // Part 1
  pos1, pos2, sc1, sc2, rolls, dieVal, turnSum: Integer;
  // Part 2
  ans2: TPair;

begin
  // Initialize frequencies for sums of 3 rolls of a 3‐sided die
  // There are 3^3=27 total outcomes
  // Sum:count = 3:1, 4:3, 5:6, 6:7, 7:6, 8:3, 9:1
  freq[3] := 1; freq[4] := 3; freq[5] := 6;
  freq[6] := 7; freq[7] := 6; freq[8] := 3; freq[9] := 1;

  // Read input.txt
  AssignFile(f, 'input.txt');
  Reset(f);
  ReadLn(f, line); // e.g. "Player 1 starting position: 4"
  p1 := StrToInt(Copy(line, Pos(':', line)+2, Length(line)));
  ReadLn(f, line);
  p2 := StrToInt(Copy(line, Pos(':', line)+2, Length(line)));
  CloseFile(f);

  // Part 1: deterministic 100‐sided die
  pos1 := p1;
  pos2 := p2;
  sc1 := 0;
  sc2 := 0;
  rolls := 0;
  dieVal := 1;
  while (sc1 < TARGET1) and (sc2 < TARGET1) do
  begin
    turnSum := 0;
    // roll three times
    turnSum := dieVal + ((dieVal mod DICE_SIDES) + 1) + (((dieVal+1) mod DICE_SIDES) + 1);
    // increment dieVal and rolls
    rolls := rolls + 3;
    dieVal := ((dieVal + 2) mod DICE_SIDES) + 1;

    if (rolls div 3) mod 2 = 1 then
    begin
      // player 1's turn
      pos1 := ((pos1 - 1 + turnSum) mod 10) + 1;
      sc1 := sc1 + pos1;
    end
    else
    begin
      // player 2's turn
      pos2 := ((pos2 - 1 + turnSum) mod 10) + 1;
      sc2 := sc2 + pos2;
    end;
  end;

  // losing score * rolls
  if sc1 >= TARGET1 then
    WriteLn((Int64(sc2) * rolls))
  else
    WriteLn((Int64(sc1) * rolls));

  // Part 2: quantum Dirac dice
  FillChar(memoVis, SizeOf(memoVis), False);
  ans2 := DiracWins(p1, p2, 0, 0, 0);
  // print the larger
  if ans2.w1 > ans2.w2 then
    WriteLn(ans2.w1)
  else
    WriteLn(ans2.w2);
end.
