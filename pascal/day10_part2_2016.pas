
program BalanceBots;

uses SysUtils; // For StrToIntDef and Int64

const
  MaxBots = 256;
  MaxOutputs = 32;

type
  TargetType = (ttBot, ttOutput);

  Target = record
    ID: integer;
    Typ: TargetType;
  end;

  Bot = record
    chips: array[1..2] of integer;
    chipCount: 0..2;
    lowTo: Target;
    highTo: Target;
    ruleDefined: boolean;
  end;

var
  bots: array[0..MaxBots-1] of Bot;
  outputs: array[0..MaxOutputs-1] of integer;
  inputFile: TextFile;
  line: string;
  i, value, botID, code: integer;
  actionTaken: boolean;
  parsePos: integer;
  tempStr, part1, part2, part3: string;
  target1Str, target2Str: string;
  lowTarget, highTarget: Target;
  result: Int64;
  lowChip, highChip : integer;


procedure ParseTarget(const s: string; var target: Target);
var
  numStr: string;
  idVal: integer;
begin
  target.ID := -1; // Default invalid ID
  if Pos('bot ', s) = 1 then
  begin
    target.Typ := ttBot;
    numStr := Copy(s, 5, Length(s) - 4);
    target.ID := StrToIntDef(numStr, -1);
  end
  else if Pos('output ', s) = 1 then
  begin
    target.Typ := ttOutput;
    numStr := Copy(s, 8, Length(s) - 7);
    target.ID := StrToIntDef(numStr, -1);
  end;
end;

procedure AddChipToBot(botIdx: integer; chipValue: integer);
begin
  if (botIdx >= 0) and (botIdx < MaxBots) then
  begin
    if bots[botIdx].chipCount < 2 then
    begin
      Inc(bots[botIdx].chipCount);
      bots[botIdx].chips[bots[botIdx].chipCount] := chipValue;
    end;
  end;
end;

procedure GiveChip(target: Target; value: integer);
begin
  if target.ID < 0 then Exit; // Invalid target parsed

  if target.Typ = ttBot then
  begin
     if target.ID < MaxBots then
       AddChipToBot(target.ID, value);
  end
  else // ttOutput
  begin
     if target.ID < MaxOutputs then
       outputs[target.ID] := value;
  end;
end;

begin
  for i := 0 to MaxBots - 1 do
  begin
    bots[i].chipCount := 0;
    bots[i].ruleDefined := false;
    bots[i].lowTo.ID := -1;
    bots[i].highTo.ID := -1;
  end;
  for i := 0 to MaxOutputs - 1 do
    outputs[i] := -1;

  Assign(inputFile, 'input.txt');
  Reset(inputFile);

  while not Eof(inputFile) do
  begin
    ReadLn(inputFile, line);
    if Pos('value ', line) = 1 then
    begin
      parsePos := Pos(' goes to bot ', line);
      if parsePos > 0 then
      begin
          part1 := Copy(line, 7, parsePos - 7);
          part2 := Copy(line, parsePos + 13, Length(line));
          value := StrToIntDef(part1, -1);
          botID := StrToIntDef(part2, -1);
          if (value <> -1) and (botID <> -1) then
              AddChipToBot(botID, value);
      end;
    end
    else if Pos('bot ', line) = 1 then
    begin
      parsePos := Pos(' gives low to ', line);
      if parsePos > 0 then
      begin
          part1 := Copy(line, 5, parsePos - 5); // Bot ID
          tempStr := Copy(line, parsePos + 14, Length(line)); // "target1 and high to target2"
          parsePos := Pos(' and high to ', tempStr);
          if parsePos > 0 then
          begin
              target1Str := Copy(tempStr, 1, parsePos - 1);
              target2Str := Copy(tempStr, parsePos + 13, Length(tempStr));
              botID := StrToIntDef(part1, -1);

              if (botID >= 0) and (botID < MaxBots) then
              begin
                  bots[botID].ruleDefined := true;
                  ParseTarget(target1Str, bots[botID].lowTo);
                  ParseTarget(target2Str, bots[botID].highTo);
              end;
          end;
      end;
    end;
  end;
  Close(inputFile);

  repeat
    actionTaken := false;
    for i := 0 to MaxBots - 1 do
    begin
      if bots[i].chipCount = 2 then
      begin
        actionTaken := true;

        if bots[i].chips[1] < bots[i].chips[2] then
        begin
          lowChip := bots[i].chips[1];
          highChip := bots[i].chips[2];
        end
        else
        begin
          lowChip := bots[i].chips[2];
          highChip := bots[i].chips[1];
        end;

        bots[i].chipCount := 0;

        if bots[i].ruleDefined then
        begin
             GiveChip(bots[i].lowTo, lowChip);
             GiveChip(bots[i].highTo, highChip);
        end;
      end;
    end;
  until not actionTaken;

  if (outputs[0] <> -1) and (outputs[1] <> -1) and (outputs[2] <> -1) then
  begin
    result := Int64(outputs[0]) * Int64(outputs[1]) * Int64(outputs[2]);
    WriteLn(result);
  end
  else
  begin
     WriteLn(0); // Or indicate error if needed
  end;

end.
