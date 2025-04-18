
program MonkeyMath;
{$mode objfpc}{$H+}
uses
  SysUtils;

const
  MAXM = 10000;  { maximum number of monkeys }

type
  TJobType = (jtNumber, jtAdd, jtSub, jtMul, jtDiv);

var
  Names: array[1..MAXM] of string;
  Job:   array[1..MAXM] of TJobType;
  Val:   array[1..MAXM] of Int64;
  Lft, Rgt: array[1..MAXM] of Integer;
  Computed: array[1..MAXM] of Boolean;
  Count: Integer;

{ Return the index for a monkey name, adding it if new }
function GetIdx(const nm: string): Integer;
var
  i: Integer;
begin
  for i := 1 to Count do
    if Names[i] = nm then
      Exit(i);
  Inc(Count);
  Names[Count] := nm;
  Result := Count;
end;

{ Recursively evaluate monkey i (with memoization) }
function Eval(i: Integer): Int64;
var
  a, b: Int64;
begin
  if Computed[i] then
    Exit(Val[i]);
  case Job[i] of
    jtNumber:
      { nothing to do };
    jtAdd:
      begin
        a := Eval(Lft[i]);
        b := Eval(Rgt[i]);
        Val[i] := a + b;
      end;
    jtSub:
      begin
        a := Eval(Lft[i]);
        b := Eval(Rgt[i]);
        Val[i] := a - b;
      end;
    jtMul:
      begin
        a := Eval(Lft[i]);
        b := Eval(Rgt[i]);
        Val[i] := a * b;
      end;
    jtDiv:
      begin
        a := Eval(Lft[i]);
        b := Eval(Rgt[i]);
        Val[i] := a div b;
      end;
  end;
  Computed[i] := True;
  Result := Val[i];
end;

var
  f: Text;
  line, name, jobstr, leftName, rightName: string;
  colonPos, spPos: Integer;
  idx, lidx, ridx: Integer;
  num: Int64;
  op: Char;
begin
  { Initialize }
  Count := 0;
  for idx := 1 to MAXM do
    Computed[idx] := False;

  { Open input.txt and read all definitions }
  Assign(f, 'input.txt');
  Reset(f);
  while not Eof(f) do
  begin
    ReadLn(f, line);
    if line = '' then Continue;

    { Split at colon }
    colonPos := Pos(':', line);
    name := Trim(Copy(line, 1, colonPos - 1));
    jobstr := Trim(Copy(line, colonPos + 1, MaxInt));

    idx := GetIdx(name);

    { Determine if it's just a number or an operation }
    if (Length(jobstr) > 0) and (jobstr[1] in ['0'..'9']) then
    begin
      { literal number }
      Job[idx] := jtNumber;
      Val[idx] := StrToInt64(jobstr);
      Computed[idx] := True;
    end
    else
    begin
      { parse "left OP right" }
      spPos := Pos(' ', jobstr);
      leftName := Copy(jobstr, 1, spPos - 1);
      op := jobstr[spPos + 1];
      rightName := Copy(jobstr, spPos + 3, MaxInt);

      lidx := GetIdx(leftName);
      ridx := GetIdx(rightName);
      Lft[idx] := lidx;
      Rgt[idx] := ridx;

      case op of
        '+': Job[idx] := jtAdd;
        '-': Job[idx] := jtSub;
        '*': Job[idx] := jtMul;
        '/': Job[idx] := jtDiv;
      end;
    end;
  end;
  Close(f);

  { Evaluate and print the value of "root" }
  idx := GetIdx('root');
  WriteLn(Eval(idx));
end.
