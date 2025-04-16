
program BathroomSecurity;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  TKeypad = array[1..3, 1..3] of Char;

const
  Keypad: TKeypad = (
    ('1', '2', '3'),
    ('4', '5', '6'),
    ('7', '8', '9')
  );

  StartingRow = 2;
  StartingCol = 2;

var
  InputFile: TextFile;
  Line: String;
  Row, Col: Integer;
  Code: String;

function GetNextDigit(Instructions: String; var Row, Col: Integer): Char;
var
  I: Integer;
  Instruction: Char;
  NewRow, NewCol: Integer;
begin
  for I := 1 to Length(Instructions) do
  begin
    Instruction := Instructions[I];
    NewRow := Row;
    NewCol := Col;

    case Instruction of
      'U': NewRow := Row - 1;
      'D': NewRow := Row + 1;
      'L': NewCol := Col - 1;
      'R': NewCol := Col + 1;
    end;

    if (NewRow >= 1) and (NewRow <= 3) and (NewCol >= 1) and (NewCol <= 3) then
    begin
      Row := NewRow;
      Col := NewCol;
    end;
  end;

  Result := Keypad[Row, Col];
end;

begin
  AssignFile(InputFile, 'input.txt');
  Reset(InputFile);

  Row := StartingRow;
  Col := StartingCol;
  Code := '';

  while not Eof(InputFile) do
  begin
    ReadLn(InputFile, Line);
    Code := Code + GetNextDigit(Line, Row, Col);
  end;

  CloseFile(InputFile);

  WriteLn(Code);
end.
