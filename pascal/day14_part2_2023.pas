
program ParabolicReflectorDish;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  TPlatform = array of array of Char;

function CalculateLoad(const Platform: TPlatform): Integer;
var
  Row, Col, Load: Integer;
begin
  Load := 0;
  for Row := Low(Platform) to High(Platform) do
  begin
    for Col := Low(Platform[Row]) to High(Platform[Row]) do
    begin
      if Platform[Row][Col] = 'O' then
      begin
        Load := Load + (High(Platform) + 1 - Row);
      end;
    end;
  end;
  CalculateLoad := Load;
end;

procedure TiltNorth(var Platform: TPlatform);
var
  Row, Col, EmptyRow: Integer;
begin
  for Col := Low(Platform[0]) to High(Platform[0]) do
  begin
    EmptyRow := Low(Platform);
    for Row := Low(Platform) to High(Platform) do
    begin
      if Platform[Row][Col] = 'O' then
      begin
        Platform[Row][Col] := '.';
        Platform[EmptyRow][Col] := 'O';
        Inc(EmptyRow);
      end
      else if Platform[Row][Col] = '#' then
      begin
        EmptyRow := Row + 1;
      end;
    end;
  end;
end;

procedure TiltWest(var Platform: TPlatform);
var
  Row, Col, EmptyCol: Integer;
begin
  for Row := Low(Platform) to High(Platform) do
  begin
    EmptyCol := Low(Platform[Row]);
    for Col := Low(Platform[Row]) to High(Platform[Row]) do
    begin
      if Platform[Row][Col] = 'O' then
      begin
        Platform[Row][Col] := '.';
        Platform[Row][EmptyCol] := 'O';
        Inc(EmptyCol);
      end
      else if Platform[Row][Col] = '#' then
      begin
        EmptyCol := Col + 1;
      end;
    end;
  end;
end;

procedure TiltSouth(var Platform: TPlatform);
var
  Row, Col, EmptyRow: Integer;
begin
  for Col := Low(Platform[0]) to High(Platform[0]) do
  begin
    EmptyRow := High(Platform);
    for Row := High(Platform) downto Low(Platform) do
    begin
      if Platform[Row][Col] = 'O' then
      begin
        Platform[Row][Col] := '.';
        Platform[EmptyRow][Col] := 'O';
        Dec(EmptyRow);
      end
      else if Platform[Row][Col] = '#' then
      begin
        EmptyRow := Row - 1;
      end;
    end;
  end;
end;

procedure TiltEast(var Platform: TPlatform);
var
  Row, Col, EmptyCol: Integer;
begin
  for Row := Low(Platform) to High(Platform) do
  begin
    EmptyCol := High(Platform[Row]);
    for Col := High(Platform[Row]) downto Low(Platform[Row]) do
    begin
      if Platform[Row][Col] = 'O' then
      begin
        Platform[Row][Col] := '.';
        Platform[Row][EmptyCol] := 'O';
        Dec(EmptyCol);
      end
      else if Platform[Row][Col] = '#' then
      begin
        EmptyCol := Col - 1;
      end;
    end;
  end;
end;

procedure Cycle(var Platform: TPlatform);
begin
  TiltNorth(Platform);
  TiltWest(Platform);
  TiltSouth(Platform);
  TiltEast(Platform);
end;

function PlatformToString(const Platform: TPlatform): string;
var
  Row, Col: Integer;
  Line: string;
begin
  Result := '';
  for Row := Low(Platform) to High(Platform) do
  begin
    Line := '';
    for Col := Low(Platform[Row]) to High(Platform[Row]) do
    begin
      Line := Line + Platform[Row][Col];
    end;
    Result := Result + Line + #13#10;
  end;
end;


function SolvePart1(const Platform: TPlatform): Integer;
var
  WorkingPlatform: TPlatform;
  Row, Col : Integer;
begin
  SetLength(WorkingPlatform, Length(Platform));
  for Row := Low(Platform) to High(Platform) do
  begin
    SetLength(WorkingPlatform[Row], Length(Platform[Row]));
    for Col := Low(Platform[Row]) to High(Platform[Row]) do
    begin
      WorkingPlatform[Row][Col] := Platform[Row][Col];
    end;
  end;

  TiltNorth(WorkingPlatform);
  SolvePart1 := CalculateLoad(WorkingPlatform);
end;


function SolvePart2(const Platform: TPlatform; NumCycles: Integer): Integer;
var
  WorkingPlatform: TPlatform;
  Row, Col: Integer;
  CycleIndex: Integer;
  SeenPlatforms: array of string;
  LoadAfterCycle: Integer;
  LoopStart: Integer;
  LoopLength: Integer;
  TargetCycle: Integer;
  StateStr: string;
begin
  SetLength(WorkingPlatform, Length(Platform));
  for Row := Low(Platform) to High(Platform) do
  begin
    SetLength(WorkingPlatform[Row], Length(Platform[Row]));
    for Col := Low(Platform[Row]) to High(Platform[Row]) do
    begin
      WorkingPlatform[Row][Col] := Platform[Row][Col];
    end;
  end;

  SetLength(SeenPlatforms, 0);

  CycleIndex := 0;
  LoopStart := -1;
  LoopLength := -1;

  while CycleIndex < NumCycles do
  begin
    StateStr := PlatformToString(WorkingPlatform);
    
    //Check if we've seen this state before
    for Row := Low(SeenPlatforms) to High(SeenPlatforms) do
    begin
      if SeenPlatforms[Row] = StateStr then
      begin
        LoopStart := Row;
        LoopLength := CycleIndex - Row;
        Break;
      end;
    end;

    if LoopStart <> -1 then
    begin
      Break;
    end;

    SetLength(SeenPlatforms, Length(SeenPlatforms) + 1);
    SeenPlatforms[High(SeenPlatforms)] := StateStr;

    Cycle(WorkingPlatform);
    Inc(CycleIndex);
  end;

  if LoopStart <> -1 then
  begin
    TargetCycle := LoopStart + ((NumCycles - LoopStart) mod LoopLength);

    // Recreate the platform at TargetCycle by cycling the original input.
    SetLength(WorkingPlatform, Length(Platform));
    for Row := Low(Platform) to High(Platform) do
    begin
      SetLength(WorkingPlatform[Row], Length(Platform[Row]));
      for Col := Low(Platform[Row]) to High(Platform[Row]) do
      begin
        WorkingPlatform[Row][Col] := Platform[Row][Col];
      end;
    end;

    for CycleIndex := 0 to TargetCycle - 1 do
    begin
       Cycle(WorkingPlatform);
    end;
  end;
  
  SolvePart2 := CalculateLoad(WorkingPlatform);
end;



var
  Platform: TPlatform;
  Line: string;
  Row, Col: Integer;
  InputFile: TextFile;

begin
  AssignFile(InputFile, 'input.txt');
  try
    Reset(InputFile);
    Row := 0;
    SetLength(Platform, 0);

    while not Eof(InputFile) do
    begin
      ReadLn(InputFile, Line);
      SetLength(Platform, Row + 1);
      SetLength(Platform[Row], Length(Line));
      for Col := 0 to Length(Line) - 1 do
      begin
        Platform[Row][Col] := Line[Col + 1];
      end;
      Inc(Row);
    end;

    CloseFile(InputFile);

    Writeln('Part 1: ', SolvePart1(Platform));
    Writeln('Part 2: ', SolvePart2(Platform, 1000000000));


  except
    on E: Exception do
      Writeln('An error occurred: ', E.Message);
  end;
end.
