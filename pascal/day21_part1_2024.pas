program Main;

{$mode objfpc}{$H+}
uses
  SysUtils;

type
  PCNode = ^TCNode;
  TCNode = record
    code: string;
    robots: integer;
    value: int64;
    next: PCNode;
  end;

const
  KeyPad: array[0..3] of string = ('789', '456', '123', ' 0A');
  RobotPad: array[0..1] of string = (' ^A', '<v>');

var
  MemoHead: PCNode;
  TotalAnswer: int64;

function FindPositionKeyPad(ch: Char; var ri, rj: Integer): Boolean;
var
  i, j: Integer;
begin
  for i := 0 to 3 do
  begin
    for j := 0 to Length(KeyPad[i]) - 1 do
      if KeyPad[i][j + 1] = ch then
      begin
        ri := i;
        rj := j;
        Result := True;
        Exit;
      end;
  end;
  Result := False;
  ri := -1;
  rj := -1;
end;

function FindPositionRobotPad(ch: Char; var ri, rj: Integer): Boolean;
var
  i, j: Integer;
begin
  for i := 0 to 1 do
  begin
    for j := 0 to Length(RobotPad[i]) - 1 do
      if RobotPad[i][j + 1] = ch then
      begin
        ri := i;
        rj := j;
        Result := True;
        Exit;
      end;
  end;
  Result := False;
  ri := -1;
  rj := -1;
end;

function RepChar(c: Char; n: Integer): string;
var
  s: string;
  i: Integer;
begin
  SetLength(s, n);
  for i := 1 to n do s[i] := c;
  Result := s;
end;

function PadOkKey(ci, cj: Integer; seq: string): Boolean;
var
  k: Integer;
  ch: Char;
begin
  for k := 1 to Length(seq) do
  begin
    if KeyPad[ci][cj + 1] = ' ' then
    begin
      Result := False;
      Exit;
    end;
    ch := seq[k];
    case ch of
      '^': Dec(ci);
      'v': Inc(ci);
      '<': Dec(cj);
      '>': Inc(cj);
    end;
    if (ci < 0) or (ci > 3) or (cj < 0) or (cj > Length(KeyPad[0]) - 1) then
    begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

function PadOkRobot(ci, cj: Integer; seq: string): Boolean;
var
  k: Integer;
  ch: Char;
begin
  for k := 1 to Length(seq) do
  begin
    if RobotPad[ci][cj + 1] = ' ' then
    begin
      Result := False;
      Exit;
    end;
    ch := seq[k];
    case ch of
      '^': Dec(ci);
      'v': Inc(ci);
      '<': Dec(cj);
      '>': Inc(cj);
    end;
    if (ci < 0) or (ci > 1) or (cj < 0) or (cj > Length(RobotPad[0]) - 1) then
    begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

function GenerateMovesKey(posI, posJ: Integer; ch: Char): string;
var
  oi, oj: Integer;
  tmp: string;
begin
  oi := -1; oj := -1;
  FindPositionKeyPad(ch, oi, oj);
  tmp := '';
  if posJ > oj then tmp := tmp + RepChar('<', posJ - oj);
  if posI > oi then tmp := tmp + RepChar('^', posI - oi);
  if posI < oi then tmp := tmp + RepChar('v', oi - posI);
  if posJ < oj then tmp := tmp + RepChar('>', oj - posJ);
  if not PadOkKey(posI, posJ, tmp) then
  begin
    tmp := '';
    if posJ < oj then tmp := tmp + RepChar('>', oj - posJ);
    if posI > oi then tmp := tmp + RepChar('^', posI - oi);
    if posI < oi then tmp := tmp + RepChar('v', oi - posI);
    if posJ > oj then tmp := tmp + RepChar('<', posJ - oj);
  end;
  Result := tmp;
end;

function GenerateMovesRobot(posI, posJ: Integer; ch: Char): string;
var
  oi, oj: Integer;
  tmp: string;
begin
  oi := -1; oj := -1;
  FindPositionRobotPad(ch, oi, oj);
  tmp := '';
  if posJ > oj then tmp := tmp + RepChar('<', posJ - oj);
  if posI > oi then tmp := tmp + RepChar('^', posI - oi);
  if posI < oi then tmp := tmp + RepChar('v', oi - posI);
  if posJ < oj then tmp := tmp + RepChar('>', oj - posJ);
  if not PadOkRobot(posI, posJ, tmp) then
  begin
    tmp := '';
    if posJ < oj then tmp := tmp + RepChar('>', oj - posJ);
    if posI > oi then tmp := tmp + RepChar('^', posI - oi);
    if posI < oi then tmp := tmp + RepChar('v', oi - posI);
    if posJ > oj then tmp := tmp + RepChar('<', posJ - oj);
  end;
  Result := tmp;
end;

function MemoGet(codeStr: string; robots: Integer; var value: int64): Boolean;
var
  cur: PCNode;
begin
  cur := MemoHead;
  while cur <> nil do
  begin
    if (cur^.code = codeStr) and (cur^.robots = robots) then
    begin
      value := cur^.value;
      Result := True;
      Exit;
    end;
    cur := cur^.next;
  end;
  Result := False;
end;

procedure MemoPut(codeStr: string; robots: Integer; value: int64);
var
  node: PCNode;
begin
  New(node);
  node^.code := codeStr;
  node^.robots := robots;
  node^.value := value;
  node^.next := MemoHead;
  MemoHead := node;
end;

function Solve(codeStr: string; robots, maxRobots: Integer): int64;
var
  i, posI, posJ: Integer;
  ch: Char;
  moves: string;
  sum, sub: int64;
  oi, oj: Integer;
begin
  if robots <= 0 then
  begin
    Result := Length(codeStr);
    Exit;
  end;

  if MemoGet(codeStr, robots, sub) then
  begin
    Result := sub;
    Exit;
  end;

  sum := 0;
  posI := 3; posJ := 2;
  if robots <> maxRobots then posI := 0;

  for i := 1 to Length(codeStr) do
  begin
    ch := codeStr[i];
    if robots = maxRobots then
    begin
      moves := GenerateMovesKey(posI, posJ, ch);
      FindPositionKeyPad(ch, oi, oj);
      posI := oi; posJ := oj;
    end
    else
    begin
      moves := GenerateMovesRobot(posI, posJ, ch);
      FindPositionRobotPad(ch, oi, oj);
      posI := oi; posJ := oj;
    end;
    sub := Solve(moves + 'A', robots - 1, maxRobots);
    sum := sum + sub;
  end;

  MemoPut(codeStr, robots, sum);
  Result := sum;
end;

var
  f: Text;
  line: string;
  trimmed: string;
  digits: int64;
  codeLine: string;
  sv: int64;
  i: Integer;
  ch: Char;
begin
  MemoHead := nil;
  TotalAnswer := 0;
  AssignFile(f, 'input.txt');
  Reset(f);
  while not Eof(f) do
  begin
    ReadLn(f, line);
    trimmed := Trim(line);
    if trimmed = '' then Continue;

    digits := 0;
    for i := 1 to Length(trimmed) do
    begin
      ch := trimmed[i];
      if (ch >= '0') and (ch <= '9') then
        digits := digits * 10 + Ord(ch) - Ord('0');
    end;

    codeLine := trimmed;
    MemoHead := nil;
    sv := Solve(codeLine, 3, 3);
    TotalAnswer := TotalAnswer + sv * digits;
  end;
  CloseFile(f);
  WriteLn(TotalAnswer);
end.