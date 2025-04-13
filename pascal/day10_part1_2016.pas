
program BotBalance;

uses
  SysUtils;

const
  MaxBots = 256; // Adjust if bot IDs exceed this
  TargetLow = 17;
  TargetHigh = 61;

type
  TTargetType = (ttBot, ttOutput);

  TBot = record
    LowToType: TTargetType;
    HighToType: TTargetType;
    LowToID: Integer;
    HighToID: Integer;
    Chip1: Integer;
    Chip2: Integer;
    ChipCount: Integer;
    Defined: Boolean; // Tracks if bot rules are set
  end;

var
  Bots: array[0..MaxBots] of TBot;
  InputFile: Text;
  Line: string;
  i: Integer;
  ActionMade, Found: Boolean;
  ResultBotID: Integer;
  Value, BotID, LowVal, HighVal: Integer;
  BotStr, LowToStr, HighToStr: string;
  P1, P2: Integer;

procedure InitializeBots;
var
  i: Integer;
begin
  for i := 0 to MaxBots do
  begin
    Bots[i].ChipCount := 0;
    Bots[i].Chip1 := -1;
    Bots[i].Chip2 := -1;
    Bots[i].Defined := False;
    Bots[i].LowToID := -1;
    Bots[i].HighToID := -1;
  end;
end;

procedure ParseTarget(TargetStr: string; var TargetType: TTargetType; var TargetID: Integer);
var
  NumStr: string;
  Code: Integer;
begin
  TargetID := -1; // Default
  if Pos('bot ', TargetStr) = 1 then
  begin
    TargetType := ttBot;
    NumStr := Copy(TargetStr, 5, Length(TargetStr) - 4);
    Val(NumStr, TargetID, Code);
  end
  else if Pos('output ', TargetStr) = 1 then
  begin
    TargetType := ttOutput;
    NumStr := Copy(TargetStr, 8, Length(TargetStr) - 7);
     Val(NumStr, TargetID, Code); // Output IDs are not used by bots array directly
  end;
   // Error handling for Val code could be added if needed
end;

procedure AddChip(BotID: Integer; ChipValue: Integer);
begin
  if (BotID >= 0) and (BotID <= MaxBots) then
  begin
     if Bots[BotID].ChipCount = 0 then
     begin
       Bots[BotID].Chip1 := ChipValue;
       Bots[BotID].ChipCount := 1;
     end
     else if Bots[BotID].ChipCount = 1 then
     begin
        // Ensure Chip1 is always <= Chip2 if needed, but problem only cares at distribution time
       Bots[BotID].Chip2 := ChipValue;
       Bots[BotID].ChipCount := 2;
     end;
      // Else: Bot already has 2 chips, ignore or error? Problem implies this won't happen before distribution
  end;
end;


begin
  InitializeBots;
  Assign(InputFile, 'input.txt');
  Reset(InputFile);

  while not Eof(InputFile) do
  begin
    ReadLn(InputFile, Line);

    if Pos('value ', Line) = 1 then
    begin
      P1 := Pos(' goes to bot ', Line);
      if P1 > 0 then
      begin
         Value := StrToIntDef(Copy(Line, 7, P1 - 7), -1);
         BotStr := Copy(Line, P1 + 13, Length(Line) - (P1 + 12));
         BotID := StrToIntDef(BotStr, -1);
         if (Value <> -1) and (BotID <> -1) then
         begin
            AddChip(BotID, Value);
         end;
      end;
    end
    else if Pos(' gives low to ', Line) > 0 then
    begin
       P1 := Pos(' gives low to ', Line);
       P2 := Pos(' and high to ', Line);
       if (P1 > 0) and (P2 > P1) then
       begin
          BotStr := Copy(Line, 5, P1-5); // Extract "bot X"
          BotID := StrToIntDef(BotStr, -1);

          LowToStr := Copy(Line, P1 + 14 , P2 - (P1 + 14));
          HighToStr := Copy(Line, P2 + 13, Length(Line) - (P2 + 12));

          if (BotID >= 0) and (BotID <= MaxBots) then
          begin
              ParseTarget(LowToStr, Bots[BotID].LowToType, Bots[BotID].LowToID);
              ParseTarget(HighToStr, Bots[BotID].HighToType, Bots[BotID].HighToID);
              Bots[BotID].Defined := True;
          end;
       end;
    end;
  end;
  Close(InputFile);

  Found := False;
  ResultBotID := -1;
  repeat
    ActionMade := False;
    for i := 0 to MaxBots do
    begin
      if Bots[i].ChipCount = 2 then
      begin
        ActionMade := True;

        if Bots[i].Chip1 < Bots[i].Chip2 then
        begin
          LowVal := Bots[i].Chip1;
          HighVal := Bots[i].Chip2;
        end
        else
        begin
          LowVal := Bots[i].Chip2;
          HighVal := Bots[i].Chip1;
        end;

        if (LowVal = TargetLow) and (HighVal = TargetHigh) then
        begin
           ResultBotID := i;
           Found := True;
           Break; // Exit inner loop
        end;

        Bots[i].ChipCount := 0; // Chips are distributed

        if Bots[i].Defined then
        begin
            if Bots[i].LowToType = ttBot then
            begin
                 AddChip(Bots[i].LowToID, LowVal);
            end;
             // else: low goes to output, ignore for this problem

             if Bots[i].HighToType = ttBot then
             begin
                 AddChip(Bots[i].HighToID, HighVal);
             end;
             // else: high goes to output, ignore for this problem
        end;
      end;
    end; // end for
     if Found then Break; // Exit outer loop
  until not ActionMade;

  if Found then
     WriteLn(ResultBotID);
  // else: Print nothing or an error message if desired

end.
