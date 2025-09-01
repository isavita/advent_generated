
program ReindeerRace;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

type
  TReindeer = record
    Speed, FlyTime, RestTime: Integer;
    Distance, Points: Integer;
    Flying: Boolean;
    TimeInMode: Integer;
  end;

var
  Reindeers: array of TReindeer;
  Lines: TStringList;
  i, j, TotalSeconds, MaxDistance, MaxPoints: Integer;
  LineTokens: TStringArray;
begin
  Lines := TStringList.Create;
  try
    if not FileExists('input.txt') then
      Halt(1);
    Lines.LoadFromFile('input.txt');
    SetLength(Reindeers, Lines.Count);
    for i := 0 to Lines.Count - 1 do
    begin
      LineTokens := Lines[i].Split([' '], TStringSplitOptions.ExcludeEmpty);
      Reindeers[i].Speed := StrToInt(LineTokens[3]);
      Reindeers[i].FlyTime := StrToInt(LineTokens[6]);
      Reindeers[i].RestTime := StrToInt(LineTokens[13]);
      Reindeers[i].Distance := 0;
      Reindeers[i].Points := 0;
      Reindeers[i].Flying := True;
      Reindeers[i].TimeInMode := 0;
    end;
  finally
    Lines.Free;
  end;

  TotalSeconds := 2503;
  for i := 1 to TotalSeconds do
  begin
    MaxDistance := 0;
    for j := 0 to High(Reindeers) do
    begin
      if Reindeers[j].Flying then
        Inc(Reindeers[j].Distance, Reindeers[j].Speed);
      Inc(Reindeers[j].TimeInMode);
      if (Reindeers[j].Flying and (Reindeers[j].TimeInMode = Reindeers[j].FlyTime)) or
         (not Reindeers[j].Flying and (Reindeers[j].TimeInMode = Reindeers[j].RestTime)) then
      begin
        Reindeers[j].Flying := not Reindeers[j].Flying;
        Reindeers[j].TimeInMode := 0;
      end;
      if Reindeers[j].Distance > MaxDistance then
        MaxDistance := Reindeers[j].Distance;
    end;
    for j := 0 to High(Reindeers) do
      if Reindeers[j].Distance = MaxDistance then
        Inc(Reindeers[j].Points);
  end;

  MaxPoints := 0;
  for i := 0 to High(Reindeers) do
    if Reindeers[i].Points > MaxPoints then
      MaxPoints := Reindeers[i].Points;

  WriteLn(MaxPoints);
end.
