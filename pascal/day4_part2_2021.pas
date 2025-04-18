program GiantSquid;
{$mode objfpc}{$H+}
uses
  SysUtils, Classes;

type
  TBoard = record
    nums: array[0..4,0..4] of Integer;
    mark: array[0..4,0..4] of Boolean;
    winOrder: Integer;
    winDraw: Integer;
  end;

var
  draws: array of Integer;
  boards: array of TBoard;

procedure ParseDraws(const line: string);
var
  parts: TStringList;
  i, v: Integer;
begin
  parts := TStringList.Create;
  try
    ExtractStrings([','], [], PChar(line), parts);
    SetLength(draws, parts.Count);
    for i := 0 to parts.Count - 1 do
    begin
      if TryStrToInt(parts[i], v) then
        draws[i] := v
      else
        draws[i] := 0;
    end;
  finally
    parts.Free;
  end;
end;

procedure ParseBoardLine(const line: string; row: Integer; var b: TBoard);
var
  parts: TStringList;
  i, v: Integer;
begin
  parts := TStringList.Create;
  try
    ExtractStrings([' '], [], PChar(Trim(line)), parts);
    for i := 0 to parts.Count - 1 do
    begin
      if TryStrToInt(parts[i], v) then
        b.nums[row, i] := v
      else
        b.nums[row, i] := 0;
      b.mark[row, i] := False;
    end;
  finally
    parts.Free;
  end;
end;

function CheckWin(var b: TBoard): Boolean;
var
  i, j: Integer;
  ok: Boolean;
begin
  // check rows
  for i := 0 to 4 do
  begin
    ok := True;
    for j := 0 to 4 do
      if not b.mark[i, j] then
      begin
        ok := False;
        Break;
      end;
    if ok then
      Exit(True);
  end;
  // check columns
  for j := 0 to 4 do
  begin
    ok := True;
    for i := 0 to 4 do
      if not b.mark[i, j] then
      begin
        ok := False;
        Break;
      end;
    if ok then
      Exit(True);
  end;
  Result := False;
end;

function ComputeScore(const b: TBoard): Int64;
var
  i, j: Integer;
  sumUnmarked: Int64;
begin
  sumUnmarked := 0;
  for i := 0 to 4 do
    for j := 0 to 4 do
      if not b.mark[i, j] then
        sumUnmarked := sumUnmarked + b.nums[i, j];
  Result := sumUnmarked * b.winDraw;
end;

var
  fline: string;
  b: TBoard;
  i, j, dIdx: Integer;
  draw: Integer;
  bestOrder, worstOrder: Integer;
  idxFirst, idxLast: Integer;
begin
  Assign(Input, 'input.txt');
  Reset(Input);
  // Read draws
  if not EOF then
  begin
    ReadLn(fline);
    ParseDraws(fline);
  end;
  // Read boards
  SetLength(boards, 0);
  while not EOF(Input) do
  begin
    ReadLn(fline);
    if Trim(fline) = '' then
      Continue;
    // first row of a board
    FillChar(b, SizeOf(b), 0);
    b.winOrder := -1;
    ParseBoardLine(fline, 0, b);
    // next 4 rows
    for i := 1 to 4 do
    begin
      ReadLn(fline);
      ParseBoardLine(fline, i, b);
    end;
    // add board
    SetLength(boards, Length(boards) + 1);
    boards[High(boards)] := b;
  end;
  // Simulate draws
  for dIdx := 0 to High(draws) do
  begin
    draw := draws[dIdx];
    for i := 0 to High(boards) do
    begin
      if boards[i].winOrder >= 0 then
        Continue;
      // mark
      for j := 0 to 4 do
        for idxFirst := 0 to 4 do
          if boards[i].nums[j, idxFirst] = draw then
            boards[i].mark[j, idxFirst] := True;
      // check win
      if CheckWin(boards[i]) then
      begin
        boards[i].winOrder := dIdx;
        boards[i].winDraw := draw;
      end;
    end;
  end;
  // find first and last winning boards
  bestOrder := MaxInt;
  worstOrder := -1;
  idxFirst := -1;
  idxLast := -1;
  for i := 0 to High(boards) do
    if boards[i].winOrder >= 0 then
    begin
      if boards[i].winOrder < bestOrder then
      begin
        bestOrder := boards[i].winOrder;
        idxFirst := i;
      end;
      if boards[i].winOrder > worstOrder then
      begin
        worstOrder := boards[i].winOrder;
        idxLast := i;
      end;
    end;
  // Output results
  if idxFirst >= 0 then
    WriteLn(ComputeScore(boards[idxFirst]))
  else
    WriteLn('No winning board found for part 1.');
  if idxLast >= 0 then
    WriteLn(ComputeScore(boards[idxLast]))
  else
    WriteLn('No winning board found for part 2.');
end.