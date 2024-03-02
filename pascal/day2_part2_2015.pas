program Day2Part2;

{$mode objfpc}{$H+}

uses
  SysUtils, // For file handling and string conversions
  Classes;  // For TStringList

function MaxValue(a, b, c: Integer): Integer; inline;
begin
  if a > b then
    Result := a
  else
    Result := b;
  if c > Result then
    Result := c;
end;

function MinPerimeter(a, b, c: Integer): Integer; inline;
var
  maxSide: Integer;
begin
  maxSide := MaxValue(a, b, c);
  // Calculate the smallest perimeter of any one face
  Result := 2 * (a + b + c - maxSide);
end;

var
  inputList: TStringList;
  i, totalRibbon, l, w, h, wrap, bow: Integer;
  dimensions: array of String;
begin
  inputList := TStringList.Create;
  try
    inputList.LoadFromFile('input.txt');
    totalRibbon := 0;

    for i := 0 to inputList.Count - 1 do
    begin
      dimensions := inputList[i].Split(['x']);
      if Length(dimensions) = 3 then
      begin
        l := StrToInt(dimensions[0]);
        w := StrToInt(dimensions[1]);
        h := StrToInt(dimensions[2]);

        wrap := MinPerimeter(l, w, h); // Calculate the smallest perimeter
        bow := l * w * h; // Calculate the bow length as the volume of the present

        totalRibbon := totalRibbon + wrap + bow;
      end;
    end;

    WriteLn('Total feet of ribbon needed: ', totalRibbon);
  finally
    inputList.Free;
  end;
end.
