
program ShuttleSearch;
{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

var
  inF: TextFile;
  line: string;
  earliest, i, busID, remt, waitt, bestWait, bestBus: LongInt;
  parts: TStringList;
begin
  { Open input.txt and read the two lines }
  AssignFile(inF, 'input.txt');
  Reset(inF);
  try
    ReadLn(inF, line);
    earliest := StrToInt(line);
    ReadLn(inF, line);
  finally
    CloseFile(inF);
  end;

  { Split the second line on commas }
  parts := TStringList.Create;
  try
    parts.Delimiter := ',';
    parts.StrictDelimiter := True;
    parts.DelimitedText := line;

    bestWait := MaxInt;
    bestBus := 0;

    { For each entry, if it’s a number compute the wait time }
    for i := 0 to parts.Count - 1 do
    begin
      if parts[i] <> 'x' then
      begin
        busID := StrToInt(parts[i]);
        remt := earliest mod busID;
        if remt = 0 then
          waitt := 0
        else
          waitt := busID - remt;

        if waitt < bestWait then
        begin
          bestWait := waitt;
          bestBus := busID;
        end;
      end;
    end;

    { Output the desired product }
    WriteLn(bestBus * bestWait);
  finally
    parts.Free;
  end;
end.
