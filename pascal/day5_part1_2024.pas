
program PrintQueue;

uses
  SysUtils;

type
  TRule = record
    X, Y: Integer;
  end;

var
  Rules: array of TRule;
  RuleCount: Integer;

  f: Text;
  line: string;

  upd: array of Integer;

  n, i, r: Integer;
  posX, posY, x, y: Integer;
  ok: Boolean;

  totalSum: Int64;

  sep: Integer;
  aVal, bVal: Integer;
  token: string;
  startPos: Integer;
  midIndex: Integer;

begin
  RuleCount := 0;
  SetLength(Rules, 0);

  totalSum := 0;

  assign(f, 'input.txt');
  Reset(f);

  // Read rules until a blank line
  while not Eof(f) do
  begin
    ReadLn(f, line);
    line := Trim(line);
    if line = '' then Break;  // end of rules section

    sep := Pos('|', line);
    if sep > 0 then
    begin
      aVal := StrToInt(Trim(Copy(line, 1, sep - 1)));
      bVal := StrToInt(Trim(Copy(line, sep + 1, Length(line) - sep)));
      SetLength(Rules, RuleCount + 1);
      Rules[RuleCount].X := aVal;
      Rules[RuleCount].Y := bVal;
      Inc(RuleCount);
    end;
  end;

  // Read updates and process
  totalSum := 0;
  while not Eof(f) do
  begin
    ReadLn(f, line);
    line := Trim(line);
    if line = '' then Continue;

    SetLength(upd, 0);
    startPos := 1;

    // Split by comma
    for i := 1 to Length(line) do
    begin
      if line[i] = ',' then
      begin
        token := Trim(Copy(line, startPos, i - startPos));
        if token <> '' then
        begin
          SetLength(upd, Length(upd) + 1);
          upd[Length(upd) - 1] := StrToInt(token);
        end;
        startPos := i + 1;
      end;
    end;

    // Last token
    if startPos <= Length(line) then
    begin
      token := Trim(Copy(line, startPos, Length(line) - startPos + 1));
      if token <> '' then
      begin
        SetLength(upd, Length(upd) + 1);
        upd[Length(upd) - 1] := StrToInt(token);
      end;
    end;

    n := Length(upd);
    if n > 0 then
    begin
      ok := True;

      // Check all rules that involve pages present in the update
      for r := 0 to RuleCount - 1 do
      begin
        x := Rules[r].X;
        y := Rules[r].Y;

        posX := 0;
        posY := 0;

        for i := 0 to n - 1 do
        begin
          if upd[i] = x then posX := i + 1;
          if upd[i] = y then posY := i + 1;
        end;

        // If both pages are present, X must come before Y
        if (posX > 0) and (posY > 0) and (posX >= posY) then
        begin
          ok := False;
          Break;
        end;
      end;

      if ok then
      begin
        midIndex := (n + 1) div 2;
        totalSum := totalSum + upd[midIndex - 1];
      end;
    end;
  end;

  WriteLn(totalSum);
  Close(f);
end.
