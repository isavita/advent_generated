program Day2Part1;

{$mode objfpc}{$H+}

uses
  SysUtils, // For file handling and string conversions
  Classes;  // For TStringList

function MinValue(a, b, c: Integer): Integer; inline;
begin
  if a < b then
    Result := a
  else
    Result := b;
  if c < Result then
    Result := c;
end;

var
  inputList: TStringList;
  i, totalPaper, l, w, h, area, slack: Integer;
  dimensions: array of String;
begin
  inputList := TStringList.Create;
  try
    inputList.LoadFromFile('input.txt');
    totalPaper := 0;

    for i := 0 to inputList.Count - 1 do
    begin
      dimensions := inputList[i].Split(['x']);
      if Length(dimensions) = 3 then
      begin
        l := StrToInt(dimensions[0]);
        w := StrToInt(dimensions[1]);
        h := StrToInt(dimensions[2]);

        area := 2*l*w + 2*w*h + 2*h*l;
        slack := MinValue(l*w, w*h, h*l);

        totalPaper := totalPaper + area + slack;
      end;
    end;

    WriteLn('Total square feet of wrapping paper needed: ', totalPaper);
  finally
    inputList.Free;
  end;
end.
