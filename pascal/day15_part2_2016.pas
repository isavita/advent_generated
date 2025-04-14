
program SolveDiscPuzzle;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  TDisc = record
    TotalPositions: Integer;
    StartPosition: Integer;
  end;
  TDiscArray = array of TDisc;

var
  InputFile: TextFile;
  Line: string;
  Discs: TDiscArray;
  DiscCount: Integer;
  P1, P2, Code: Integer;
  NumStr: string;
  TotalP, StartP: Integer;
  Time: Int64;
  Found: Boolean;
  i: Integer;
  AllPass: Boolean;
  CurrentDisc: TDisc;

begin
  AssignFile(InputFile, 'input.txt');
  Reset(InputFile);

  DiscCount := 0;
  SetLength(Discs, 0);

  while not Eof(InputFile) do
  begin
    ReadLn(InputFile, Line);

    P1 := Pos(' has ', Line);
    P2 := Pos(' positions;', Line);
    if (P1 > 0) and (P2 > P1) then
    begin
      NumStr := Copy(Line, P1 + Length(' has '), P2 - (P1 + Length(' has ')));
      Val(Trim(NumStr), TotalP, Code);
      if Code <> 0 then Halt(1); // Exit on parsing error
    end else Halt(1);


    P1 := Pos(' position ', Line);
    P2 := Pos('.', Line);
     if (P1 > 0) and (P2 > P1) then
    begin
       NumStr := Copy(Line, P1 + Length(' position '), P2 - (P1 + Length(' position ')));
       Val(Trim(NumStr), StartP, Code);
       if Code <> 0 then Halt(1); // Exit on parsing error
    end else Halt(1);


    SetLength(Discs, DiscCount + 1);
    Discs[DiscCount].TotalPositions := TotalP;
    Discs[DiscCount].StartPosition := StartP;
    Inc(DiscCount);

  end;
  CloseFile(InputFile);

  SetLength(Discs, DiscCount + 1);
  Discs[DiscCount].TotalPositions := 11;
  Discs[DiscCount].StartPosition := 0;
  Inc(DiscCount);


  Time := 0;
  Found := False;
  while not Found do
  begin
    AllPass := True;
    for i := 0 to High(Discs) do
    begin
      CurrentDisc := Discs[i];
      if (CurrentDisc.StartPosition + Time + i + 1) mod CurrentDisc.TotalPositions <> 0 then
      begin
        AllPass := False;
        Break;
      end;
    end;

    if AllPass then
    begin
      Found := True;
    end
    else
    begin
      Inc(Time);
    end;
  end;

  WriteLn(Time);

end.
