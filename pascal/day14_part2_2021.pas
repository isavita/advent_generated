program Polymer;

uses
  SysUtils;

type
  TRule = record
    First: Integer;
    Second: Integer;
    Insert: Integer;
  end;
  TRuleArr = array of TRule;

var
  Template: string;
  Rules: TRuleArr;
  RuleCount: Integer;

  i, j, step: Integer;
  first, second, ins: Integer;
  lenT: Integer;
  count: Int64;
  lastIndex: Integer;
  MaxCount: Int64;
  MinCount: Int64;

  PairCounts: array[0..25,0..25] of Int64;
  NewPairCounts: array[0..25,0..25] of Int64;
  ElementCounts: array[0..25] of Int64;

function LettersOnly(s: string): string;
var
  k, n: Integer;
  res: string;
begin
  res := '';
  for k := 1 to Length(s) do
    if (s[k] >= 'A') and (s[k] <= 'Z') then
      res := res + s[k];
  LettersOnly := res;
end;

procedure ReadInputFile(const FileName: string; var TemplateOut: string; var RulesOut: TRuleArr; var RuleCountOut: Integer);
var
  f: Text;
  line: string;
  letters: string;
begin
  Assign(f, FileName);
  Reset(f);
  Readln(f, TemplateOut);
  RuleCountOut := 0;
  SetLength(RulesOut, 0);
  while not Eof(f) do
  begin
    Readln(f, line);
    letters := LettersOnly(line);
    if Length(letters) >= 3 then
    begin
      SetLength(RulesOut, RuleCountOut + 1);
      RulesOut[RuleCountOut].First := Ord(letters[1]) - Ord('A');
      RulesOut[RuleCountOut].Second := Ord(letters[2]) - Ord('A');
      RulesOut[RuleCountOut].Insert := Ord(letters[3]) - Ord('A');
      Inc(RuleCountOut);
    end;
  end;
  Close(f);
end;

begin
  ReadInputFile('input.txt', Template, Rules, RuleCount);

  FillChar(PairCounts, SizeOf(PairCounts), 0);

  lenT := Length(Template);
  for i := 1 to lenT - 1 do
  begin
    first := Ord(Template[i]) - Ord('A');
    second := Ord(Template[i + 1]) - Ord('A');
    PairCounts[first, second] := PairCounts[first, second] + 1;
  end;

  for step := 1 to 40 do
  begin
    FillChar(NewPairCounts, SizeOf(NewPairCounts), 0);
    for i := 0 to RuleCount - 1 do
    begin
      first := Rules[i].First;
      second := Rules[i].Second;
      ins := Rules[i].Insert;
      count := PairCounts[first, second];
      if count <> 0 then
      begin
        NewPairCounts[first, ins] := NewPairCounts[first, ins] + count;
        NewPairCounts[ins, second] := NewPairCounts[ins, second] + count;
      end;
    end;
    for i := 0 to 25 do
      for j := 0 to 25 do
        PairCounts[i, j] := NewPairCounts[i, j];
  end;

  FillChar(ElementCounts, SizeOf(ElementCounts), 0);
  for i := 0 to 25 do
    for j := 0 to 25 do
      ElementCounts[i] := ElementCounts[i] + PairCounts[i, j];

  lastIndex := Ord(Template[lenT]) - Ord('A');
  ElementCounts[lastIndex] := ElementCounts[lastIndex] + 1;

  MaxCount := 0;
  MinCount := High(Int64);
  for i := 0 to 25 do
  begin
    if ElementCounts[i] > MaxCount then
      MaxCount := ElementCounts[i];
    if (ElementCounts[i] > 0) and (ElementCounts[i] < MinCount) then
      MinCount := ElementCounts[i];
  end;

  WriteLn(MaxCount - MinCount);
end.