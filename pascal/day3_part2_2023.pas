program Solution;

uses SysUtils;

const
  MAX_PARTS = 2000;
  NEI = 8;

  dx: array[0..NEI-1] of Integer = (0, 0, 1, -1, -1, -1, 1, 1);
  dy: array[0..NEI-1] of Integer = (1, -1, 0, 0, -1, 1, -1, 1);

type
  TPart = record
    xmin, xmax, y, n: Integer;
  end;

var
  f: TextFile;
  lines: array of string;
  gridWidth, gridHeight: Integer;
  parts: array[0..MAX_PARTS-1] of TPart;
  partsCount: Integer;
  inCurr: Boolean;
  curr: TPart;
  i, lenLine, j, x: Integer;
  lineStr: string;
  sum: Int64;
  partsGrid: array of Integer;
  neighbor: array[0..1999] of Integer;
  neighborCount: Integer;
  gridSize: Integer;
  nx, ny, pIndex, t, s: Integer;
  found: Boolean;
  prod64: Int64;

begin
  assign(f, 'input.txt');
  {$I-}
  Reset(f);
  {$I+}
  if IOResult <> 0 then begin
    WriteLn(0);
    Halt;
  end;

  gridWidth := 0;
  gridHeight := 0;
  setlength(lines, 0);
  partsCount := 0;
  inCurr := False;

  while not Eof(f) do begin
    ReadLn(f, lineStr);
    if gridWidth = 0 then
      gridWidth := Length(lineStr);
    setlength(lines, gridHeight + 1);
    lines[gridHeight] := lineStr;

    lenLine := Length(lineStr);
    for i := 1 to lenLine do begin
      if (lineStr[i] >= '0') and (lineStr[i] <= '9') then begin
        if not inCurr then begin
          inCurr := True;
          curr.y := gridHeight;
          curr.xmin := i - 1;
          curr.xmax := i - 1;
          curr.n := Ord(lineStr[i]) - Ord('0');
        end
        else begin
          curr.n := curr.n * 10 + (Ord(lineStr[i]) - Ord('0'));
          curr.xmax := i - 1;
        end;
      end
      else begin
        if inCurr then begin
          if partsCount < MAX_PARTS then begin
            parts[partsCount] := curr;
            inc(partsCount);
          end;
          inCurr := False;
        end;
      end;
    end;

    gridHeight := gridHeight + 1;

    if inCurr then begin
      if partsCount < MAX_PARTS then begin
        parts[partsCount] := curr;
        inc(partsCount);
      end;
      inCurr := False;
    end;
  end;

  Close(f);

  gridSize := gridWidth * gridHeight;
  setlength(partsGrid, gridSize);
  for i := 0 to gridSize - 1 do
    partsGrid[i] := -1;

  for i := 0 to partsCount - 1 do begin
    for x := parts[i].xmin to parts[i].xmax do begin
      partsGrid[parts[i].y * gridWidth + x] := i;
    end;
  end;

  sum := 0;
  for i := 0 to gridHeight - 1 do begin
    for j := 0 to gridWidth - 1 do begin
      if lines[i][j+1] = '*' then begin
        neighborCount := 0;
        for t := 0 to NEI - 1 do begin
          nx := j + dx[t];
          ny := i + dy[t];
          if (nx >= 0) and (nx < gridWidth) and (ny >= 0) and (ny < gridHeight) then begin
            pIndex := partsGrid[ny * gridWidth + nx];
            if pIndex <> -1 then begin
              found := False;
              for s := 0 to neighborCount - 1 do begin
                if neighbor[s] = pIndex then begin
                  found := True;
                  Break;
                end;
              end;
              if not found then begin
                neighbor[neighborCount] := pIndex;
                inc(neighborCount);
              end;
            end;
          end;
        end;
        if neighborCount = 2 then begin
          prod64 := Int64(parts[neighbor[0]].n) * Int64(parts[neighbor[1]].n);
          sum := sum + prod64;
        end;
      end;
    end;
  end;

  WriteLn(sum);
end.