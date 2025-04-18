program SpaceImageFormat;

{$APPTYPE CONSOLE}

uses
  SysUtils;

const
  Width = 25;
  Height = 6;

var
  inputFile: Text;
  s: AnsiString;
  layerSize, layerCount, i, j, idx: Integer;
  minZeroCount, zeroCount, oneCount, twoCount, checksum: Integer;
  finalImg: array[1..Width * Height] of Char;
  ch: Char;
  row, col: Integer;
begin
  { Read the entire image data from input.txt as one line }
  Assign(inputFile, 'input.txt');
  Reset(inputFile);
  if not Eof(inputFile) then
    ReadLn(inputFile, s)
  else
    s := '';
  Close(inputFile);

  layerSize := Width * Height;
  if (Length(s) mod layerSize) <> 0 then
  begin
    Writeln('Error: input size is not a multiple of layer size.');
    Halt(1);
  end;
  layerCount := Length(s) div layerSize;

  { Part 1: find layer with fewest '0' digits }
  minZeroCount := MaxInt;
  checksum := 0;
  for i := 0 to layerCount - 1 do
  begin
    zeroCount := 0;
    oneCount  := 0;
    twoCount  := 0;
    idx := i * layerSize;
    for j := 1 to layerSize do
    begin
      ch := s[idx + j];
      case ch of
        '0': Inc(zeroCount);
        '1': Inc(oneCount);
        '2': Inc(twoCount);
      end;
    end;
    if zeroCount < minZeroCount then
    begin
      minZeroCount := zeroCount;
      checksum := oneCount * twoCount;
    end;
  end;

  Writeln('Part 1: ', checksum);

  { Part 2: decode final image by stacking layers }
  { Initialize final image to transparent ('2') }
  for i := 1 to layerSize do
    finalImg[i] := '2';

  { For each layer, set pixel if still transparent }
  for i := 0 to layerCount - 1 do
  begin
    idx := i * layerSize;
    for j := 1 to layerSize do
      if finalImg[j] = '2' then
        finalImg[j] := s[idx + j];
  end;

  Writeln('Part 2: Decoded Image');
  for row := 0 to Height - 1 do
  begin
    for col := 1 to Width do
    begin
      ch := finalImg[row * Width + col];
      { Map 0 -> space, 1 -> hash for visibility }
      if ch = '1' then
        Write('#')
      else
        Write(' ');
    end;
    Writeln;
  end;
end.