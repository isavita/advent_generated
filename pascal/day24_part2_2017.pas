
program ElectromagneticMoat;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  TComponent = record
    Port1: Integer;
    Port2: Integer;
    Used: Boolean;
  end;

var
  Components: array of TComponent;
  ComponentCount: Integer;
  MaxStrength: Integer;
  MaxLength: Integer;
  MaxLengthStrength: Integer;

procedure ReadInputFromFile(const FileName: string);
var
  F: TextFile;
  Line: string;
  Port1, Port2: Integer;
begin
  AssignFile(F, FileName);
  Reset(F);
  ComponentCount := 0;
  while not Eof(F) do
  begin
    ReadLn(F, Line);
    if Trim(Line) <> '' then
    begin
      Inc(ComponentCount);
      SetLength(Components, ComponentCount);

      if Pos('/', Line) > 0 then
      begin
        Port1 := StrToInt(Copy(Line, 1, Pos('/', Line) - 1));
        Port2 := StrToInt(Copy(Line, Pos('/', Line) + 1, Length(Line)));
        Components[ComponentCount - 1].Port1 := Port1;
        Components[ComponentCount - 1].Port2 := Port2;
        Components[ComponentCount - 1].Used := False;
      end else
      begin
         WriteLn('Invalid input line: ', Line);
         Halt(1);
      end;

    end;
  end;
  CloseFile(F);
end;

procedure FindStrongestBridge(CurrentPort: Integer; CurrentStrength: Integer; CurrentLength: Integer);
var
  i: Integer;
begin
  if CurrentStrength > MaxStrength then
    MaxStrength := CurrentStrength;

  if CurrentLength > MaxLength then
  begin
    MaxLength := CurrentLength;
    MaxLengthStrength := CurrentStrength;
  end else if CurrentLength = MaxLength then
  begin
    if CurrentStrength > MaxLengthStrength then
      MaxLengthStrength := CurrentStrength;
  end;

  for i := 0 to ComponentCount - 1 do
  begin
    if not Components[i].Used then
    begin
      if Components[i].Port1 = CurrentPort then
      begin
        Components[i].Used := True;
        FindStrongestBridge(Components[i].Port2, CurrentStrength + Components[i].Port1 + Components[i].Port2, CurrentLength + 1);
        Components[i].Used := False;
      end else if Components[i].Port2 = CurrentPort then
      begin
        Components[i].Used := True;
        FindStrongestBridge(Components[i].Port1, CurrentStrength + Components[i].Port1 + Components[i].Port2, CurrentLength + 1);
        Components[i].Used := False;
      end;
    end;
  end;
end;

var
  StartTime, EndTime: TDateTime;

begin
  StartTime := Now;

  ReadInputFromFile('input.txt');

  MaxStrength := 0;
  MaxLength := 0;
  MaxLengthStrength := 0;

  FindStrongestBridge(0, 0, 0);

  EndTime := Now;

  Writeln('Part 1: Strongest bridge strength: ', MaxStrength);
  Writeln('Part 2: Longest bridge strength: ', MaxLengthStrength);

  Writeln('Execution Time: ', FormatDateTime('ss.zzz', EndTime - StartTime), ' seconds');

end.
