program Solution;

uses sysutils, classes;

var
  inputFile: Text;
  line: string;
  lines: TStringList;
  player1Start, player2Start, player1Pos, player2Pos: integer;
  player1Score, player2Score, dieRoll, rollCount, rolls: integer;

begin
  Assign(inputFile, 'input.txt');
  Reset(inputFile);

  lines := TStringList.Create;
  while not Eof(inputFile) do
  begin
    Readln(inputFile, line);
    lines.Add(line);
  end;
  
  player1Start := StrToInt(Copy(lines[0], 29, Length(lines[0])));
  player2Start := StrToInt(Copy(lines[1], 29, Length(lines[1])));
  player1Pos := player1Start;
  player2Pos := player2Start;
  player1Score := 0;
  player2Score := 0;
  dieRoll := 1;
  rollCount := 0;

  repeat
    // Player 1
    rolls := dieRoll mod 100 + (dieRoll + 1) mod 100 + (dieRoll + 2) mod 100;
    rollCount := rollCount + 3;
    dieRoll := dieRoll + 3;

    player1Pos := (player1Pos + rolls - 1) mod 10 + 1;
    player1Score := player1Score + player1Pos;

    if player1Score >= 1000 then
    begin
      Writeln('Result:', player2Score * rollCount);
      Break;
    end;

    // Player 2
    rolls := dieRoll mod 100 + (dieRoll + 1) mod 100 + (dieRoll + 2) mod 100;
    rollCount := rollCount + 3;
    dieRoll := dieRoll + 3;

    player2Pos := (player2Pos + rolls - 1) mod 10 + 1;
    player2Score := player2Score + player2Pos;

    if player2Score >= 1000 then
    begin
      Writeln(player1Score * rollCount);
      Break;
    end;
  until False;

  Close(inputFile);
end.