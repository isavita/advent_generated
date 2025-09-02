
program Solve;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

const
  MAX_BLUEPRINTS = 30;
  MEMOSIZE = 65536;
  INF = Low(Integer);

type
  Blueprint = record
    id, oreForOreRobot, oreForClayRobot, oreForObsidianRobot,
    clayForObsidianRobot, oreForGeodeRobot, obsidianForGeodeRobot: Integer;
  end;

  State = record
    blueprint: Blueprint;
    ore, clay, obsidian, geode: Integer;
    oreRobots, clayRobots, obsidianRobots, geodeRobots: Integer;
  end;

var
  memo: array[0..MEMOSIZE-1] of Integer;

function NewState(const bp: Blueprint): State;
begin
  Result.blueprint := bp;
  Result.ore := 0; Result.clay := 0; Result.obsidian := 0; Result.geode := 0;
  Result.oreRobots := 1; Result.clayRobots := 0; Result.obsidianRobots := 0; Result.geodeRobots := 0;
end;

procedure Farm(var s: State);
begin
  s.ore := s.ore + s.oreRobots;
  s.clay := s.clay + s.clayRobots;
  s.obsidian := s.obsidian + s.obsidianRobots;
  s.geode := s.geode + s.geodeRobots;
end;

function Hash(const s: State; time: Integer): UInt64;
begin
  Result := UInt64(time);
  Result := Result*31 + UInt64(s.ore);
  Result := Result*31 + UInt64(s.clay);
  Result := Result*31 + UInt64(s.obsidian);
  Result := Result*31 + UInt64(s.geode);
  Result := Result*31 + UInt64(s.oreRobots);
  Result := Result*31 + UInt64(s.clayRobots);
  Result := Result*31 + UInt64(s.obsidianRobots);
  Result := Result*31 + UInt64(s.geodeRobots);
end;

function MaxInt(a,b: Integer): Integer; inline;
begin
  if a>b then Result:=a else Result:=b;
end;

function MinInt(a,b: Integer): Integer; inline;
begin
  if a<b then Result:=a else Result:=b;
end;

function CalcMostGeodes(s: State; time,totalTime,earliestGeode: Integer): Integer;
var
  h: UInt64;
  idx: Integer;
  cp: State;
  val: Integer;
begin
  if time=totalTime then exit(s.geode);
  h:=Hash(s,time);
  idx:=Integer(h mod MEMOSIZE);
  if memo[idx]<>INF then
    if (UInt64(memo[idx] shr 10)=h) then exit(memo[idx] and $3FF);
  if (s.geode=0) and (time>earliestGeode) then exit(0);
  Result:=s.geode;
  if (s.ore>=s.blueprint.oreForGeodeRobot) and (s.obsidian>=s.blueprint.obsidianForGeodeRobot) then
  begin
    cp:=s;
    Farm(cp);
    cp.ore:=cp.ore - cp.blueprint.oreForGeodeRobot;
    cp.obsidian:=cp.obsidian - cp.blueprint.obsidianForGeodeRobot;
    cp.geodeRobots:=cp.geodeRobots+1;
    if cp.geodeRobots=1 then earliestGeode:=MinInt(earliestGeode,time+1);
    val:=CalcMostGeodes(cp,time+1,totalTime,earliestGeode);
    Result:=MaxInt(Result,val);
  end else
  begin
    if (time<=totalTime-16) and (s.oreRobots<s.blueprint.oreForObsidianRobot*2) and (s.ore>=s.blueprint.oreForOreRobot) then
    begin
      cp:=s;
      cp.ore:=cp.ore - cp.blueprint.oreForOreRobot;
      Farm(cp);
      cp.oreRobots:=cp.oreRobots+1;
      Result:=MaxInt(Result,CalcMostGeodes(cp,time+1,totalTime,earliestGeode));
    end;
    if (time<=totalTime-8) and (s.clayRobots<s.blueprint.clayForObsidianRobot) and (s.ore>=s.blueprint.oreForClayRobot) then
    begin
      cp:=s;
      cp.ore:=cp.ore - cp.blueprint.oreForClayRobot;
      Farm(cp);
      cp.clayRobots:=cp.clayRobots+1;
      Result:=MaxInt(Result,CalcMostGeodes(cp,time+1,totalTime,earliestGeode));
    end;
    if (time<=totalTime-4) and (s.obsidianRobots<s.blueprint.obsidianForGeodeRobot) and
       (s.ore>=s.blueprint.oreForObsidianRobot) and (s.clay>=s.blueprint.clayForObsidianRobot) then
    begin
      cp:=s;
      cp.ore:=cp.ore - cp.blueprint.oreForObsidianRobot;
      cp.clay:=cp.clay - cp.blueprint.clayForObsidianRobot;
      Farm(cp);
      cp.obsidianRobots:=cp.obsidianRobots+1;
      Result:=MaxInt(Result,CalcMostGeodes(cp,time+1,totalTime,earliestGeode));
    end;
    cp:=s;
    Farm(cp);
    Result:=MaxInt(Result,CalcMostGeodes(cp,time+1,totalTime,earliestGeode));
  end;
  memo[idx]:=(Integer(h) shl 10) or (Result and $3FF);
end;

function Part1(const blueprints: array of Blueprint; n: Integer): Integer;
var
  i, geodes, sum: Integer;
  st: State;
begin
  sum:=0;
  for i:=0 to n-1 do
  begin
    FillChar(memo,SizeOf(memo),$FF);
    st:=NewState(blueprints[i]);
    geodes:=CalcMostGeodes(st,0,24,24);
    sum:=sum + st.blueprint.id*geodes;
  end;
  Result:=sum;
end;

procedure ParseLine(const line: string; out bp: Blueprint);
var
  nums: array[0..6] of Integer;
  cnt, i, len, v: Integer;
  ch: Char;
begin
  cnt:=0; v:=0; len:=Length(line);
  for i:=1 to len do
  begin
    ch:=line[i];
    if ch in ['0'..'9'] then
      v:=v*10+Ord(ch)-48
    else if v>0 then
    begin
      nums[cnt]:=v; Inc(cnt); v:=0;
      if cnt=7 then Break;
    end;
  end;
  if v>0 then begin nums[cnt]:=v; Inc(cnt); end;
  bp.id:=nums[0];
  bp.oreForOreRobot:=nums[1];
  bp.oreForClayRobot:=nums[2];
  bp.oreForObsidianRobot:=nums[3];
  bp.clayForObsidianRobot:=nums[4];
  bp.oreForGeodeRobot:=nums[5];
  bp.obsidianForGeodeRobot:=nums[6];
end;

var
  f: TextFile;
  line: string;
  blueprints: array[0..MAX_BLUEPRINTS-1] of Blueprint;
  cnt: Integer;
begin
  AssignFile(f,'input.txt');
  Reset(f);
  cnt:=0;
  while not Eof(f) do
  begin
    Readln(f,line);
    ParseLine(line,blueprints[cnt]);
    Inc(cnt);
  end;
  CloseFile(f);
  Writeln(Part1(blueprints,cnt));
end.
