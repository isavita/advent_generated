program Main;

{$mode objfpc}{$h+}
uses
  SysUtils;

type
  TImage = array of string;

function CalculateIndex(i, j: integer; const image: TImage; rows, cols: integer; flip: boolean): integer;
var
  idx, di, dj, ni, nj: integer;
begin
  idx := 0;
  for di := -1 to 1 do
    for dj := -1 to 1 do
    begin
      idx := idx * 2;
      ni := i + di;
      nj := j + dj;
      if (ni >= 0) and (ni < rows) and (nj >= 0) and (nj < cols) then
      begin
        if image[ni][nj + 1] = '#' then
          idx := idx or 1;
      end
      else if flip then
        idx := idx or 1;
    end;
  Result := idx;
end;

function ApplyAlgorithm(const image: TImage; rows, cols: integer; const algorithm: string; flip: boolean; var new_rows: integer; var new_cols: integer): TImage;
var
  i, j, idx: integer;
  ch: Char;
  enhanced: TImage;
  r, c: integer;
begin
  new_rows := rows + 2;
  new_cols := cols + 2;
  SetLength(enhanced, new_rows);
  for r := 0 to new_rows - 1 do
  begin
    SetLength(enhanced[r], new_cols);
    for c := 1 to new_cols do
      enhanced[r][c] := ' ';
  end;

  for i := 0 to new_rows - 1 do
    for j := 0 to new_cols - 1 do
    begin
      idx := CalculateIndex(i - 1, j - 1, image, rows, cols, flip);
      ch := algorithm[idx + 1];
      enhanced[i][j + 1] := ch;
    end;

  Result := enhanced;
end;

function EnhanceImage(const image: TImage; rows, cols: integer; const algorithm: string; times: integer; var final_rows: integer; var final_cols: integer): TImage;
var
  i: integer;
  currImage, temp: TImage;
  currRows, currCols: integer;
  tRows, tCols: integer;
  flip: boolean;
begin
  currImage := image;
  currRows := rows;
  currCols := cols;
  for i := 0 to times - 1 do
  begin
    flip := ((i mod 2) = 1) and (algorithm[1] = '#');
    temp := ApplyAlgorithm(currImage, currRows, currCols, algorithm, flip, tRows, tCols);
    currImage := temp;
    currRows := tRows;
    currCols := tCols;
  end;
  final_rows := currRows;
  final_cols := currCols;
  Result := currImage;
end;

function CountLitPixels(const image: TImage; rows, cols: integer): integer;
var
  i, j, cnt: integer;
begin
  cnt := 0;
  for i := 0 to rows - 1 do
    for j := 0 to cols - 1 do
      if image[i][j + 1] = '#' then
        inc(cnt);
  Result := cnt;
end;

procedure LoadInput(const fileName: string; out algorithm: string; out image: TImage; out rows: integer; out cols: integer);
var
  f: Text;
  line: string;
begin
  AssignFile(f, fileName);
  Reset(f);
  ReadLn(f, algorithm);
  SetLength(image, 0);
  rows := 0;
  cols := 0;
  while not Eof(f) do
  begin
    ReadLn(f, line);
    if Length(line) <= 1 then continue;
    if rows = 0 then
      cols := Length(line)
    else if Length(line) <> cols then
    begin
      SetLength(image, 0);
      rows := 0;
      Break;
    end;
    SetLength(image, rows + 1);
    image[rows] := line;
    Inc(rows);
  end;
  CloseFile(f);
end;

var
  algorithm: string;
  image: TImage;
  rows, cols: integer;
  newRows, newCols: integer;

begin
  LoadInput('input.txt', algorithm, image, rows, cols);
  image := EnhanceImage(image, rows, cols, algorithm, 2, newRows, newCols);
  WriteLn(CountLitPixels(image, newRows, newCols));
end.