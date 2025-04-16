
program CampCleanup;

{$MODE DELPHI}

uses
  SysUtils;

type
  TRange = record
    Start, EndValue: Integer;
  end;

function ReadRange(const s: string): TRange;
var
  DashPos: Integer;
begin
  DashPos := Pos('-', s);
  if DashPos > 0 then
  begin
    Result.Start := StrToInt(Copy(s, 1, DashPos - 1));
    Result.EndValue := StrToInt(Copy(s, DashPos + 1, Length(s)));
  end else begin
      Result.Start := 0;
      Result.EndValue := 0;
  end;
end;

function RangeContains(const r1, r2: TRange): Boolean;
begin
  Result := (r1.Start <= r2.Start) and (r1.EndValue >= r2.EndValue);
end;

function OneRangeContainsOther(const r1, r2: TRange): Boolean;
begin
  Result := RangeContains(r1, r2) or RangeContains(r2, r1);
end;

var
  InputFile: TextFile;
  Line: string;
  CommaPos: Integer;
  Range1Str, Range2Str: string;
  Range1, Range2: TRange;
  Count: Integer;
begin
  AssignFile(InputFile, 'input.txt');
  Reset(InputFile);
  Count := 0;

  try
    while not Eof(InputFile) do
    begin
      ReadLn(InputFile, Line);
      CommaPos := Pos(',', Line);

      if CommaPos > 0 then
      begin
        Range1Str := Copy(Line, 1, CommaPos - 1);
        Range2Str := Copy(Line, CommaPos + 1, Length(Line));

        Range1 := ReadRange(Range1Str);
        Range2 := ReadRange(Range2Str);

        if OneRangeContainsOther(Range1, Range2) then
        begin
          Inc(Count);
        end;
      end;
    end;
  finally
    CloseFile(InputFile);
  end;

  WriteLn(Count);
end.
