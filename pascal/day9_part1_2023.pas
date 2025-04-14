
program Solve;

{$mode delphi}{$H+}

uses
  SysUtils;

type
  TIntArray = array of LongInt; // Use LongInt for potentially larger numbers
  THistories = array of TIntArray;

function FetchInt(var S: string): LongInt;
var
  P: Integer;
  V: LongInt;
  Code: Integer;
begin
  S := TrimLeft(S);
  P := 1;
  while (P <= Length(S)) and (S[P] <> ' ') do
    Inc(P);
  Val(Copy(S, 1, P - 1), V, Code);
  // Assume valid input, no error check on Code
  Result := V;
  Delete(S, 1, P - 1);
  S := TrimLeft(S);
end;


function ParseLine(const line: string): TIntArray;
var
  tempLine: string;
  count: Integer;
  num: LongInt;
begin
  tempLine := line + ' '; // Add delimiter for easier parsing
  count := 0;
  SetLength(Result, 0);

  while Length(tempLine) > 0 do
  begin
      num := FetchInt(tempLine);
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := num;
  end;
end;

function AllZeros(const nums: TIntArray): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to High(nums) do
  begin
    if nums[i] <> 0 then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function CalculateExtrapolation(const history: TIntArray): TIntArray;
var
  i: Integer;
  len: Integer;
begin
  len := Length(history);
  if len <= 1 then
  begin
      SetLength(Result, 0);
      Exit;
  end;

  SetLength(Result, len - 1);
  for i := 0 to High(Result) do
  begin
    Result[i] := history[i + 1] - history[i];
  end;
end;

function SolveHistories(const histories: THistories): Int64;
var
  history, currentHistory: TIntArray;
  totalSum, prediction: Int64;
  i: Integer;

begin
  totalSum := 0;
  for i := 0 to High(histories) do
  begin
    history := histories[i];
    prediction := 0;
    currentHistory := history;

    while Length(currentHistory) > 0 do
    begin
        prediction := prediction + currentHistory[High(currentHistory)];
        if AllZeros(currentHistory) then
            Break;
        currentHistory := CalculateExtrapolation(currentHistory);
    end;
    totalSum := totalSum + prediction;
  end;
  Result := totalSum;
end;

var
  inputFile: TextFile;
  line: string;
  allHistories: THistories;
  finalResult: Int64;
  historyCount: Integer;

begin
  Assign(inputFile, 'input.txt');
  try
    Reset(inputFile);
    historyCount := 0;
    SetLength(allHistories, 0);

    while not Eof(inputFile) do
    begin
      ReadLn(inputFile, line);
       line := Trim(line);
      if Length(line) > 0 then
      begin
        SetLength(allHistories, historyCount + 1);
        allHistories[historyCount] := ParseLine(line);
        Inc(historyCount);
      end;
    end;
  finally
    CloseFile(inputFile);
  end;

  finalResult := SolveHistories(allHistories);
  WriteLn(finalResult);
end.
