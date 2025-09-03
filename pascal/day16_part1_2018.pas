program ChronalClassification;

uses SysUtils;

type
  TRegs = array[0..3] of Longint;

procedure ParseLineTo4(const s: string; var v: TRegs);
var
  i, idx: Integer;
  cur: string;
  ch: Char;
begin
  idx := 0;
  cur := '';
  for i := 1 to Length(s) do begin
    ch := s[i];
    if (((ch >= '0') and (ch <= '9')) or (ch = '-')) then begin
      cur := cur + ch;
    end else begin
      if cur <> '' then begin
        if idx < 4 then v[idx] := StrToInt(cur);
        Inc(idx);
        cur := '';
      end;
    end;
  end;
  if cur <> '' then begin
    if idx < 4 then v[idx] := StrToInt(cur);
    Inc(idx);
  end;
end;

procedure ApplyOp(op: Integer; A, B, C: Integer; var regs: TRegs);
begin
  case op of
    0: regs[C] := regs[A] + regs[B];        // addr
    1: regs[C] := regs[A] + B;               // addi
    2: regs[C] := regs[A] * regs[B];        // mulr
    3: regs[C] := regs[A] * B;               // muli
    4: regs[C] := regs[A] and regs[B];       // banr
    5: regs[C] := regs[A] and B;              // bani
    6: regs[C] := regs[A] or regs[B];        // borr
    7: regs[C] := regs[A] or B;               // bori
    8: regs[C] := regs[A];                    // setr
    9: regs[C] := A;                           // seti
    10: regs[C] := Ord(A > regs[B]);          // gtir
    11: regs[C] := Ord(regs[A] > B);          // gtri
    12: regs[C] := Ord(regs[A] > regs[B]);    // gtrr
    13: regs[C] := Ord(A = regs[B]);          // eqir
    14: regs[C] := Ord(regs[A] = B);          // eqri
    15: regs[C] := Ord(regs[A] = regs[B]);    // eqrr
  end;
end;

function RegsEqual(const a, b: TRegs): Boolean;
begin
  RegsEqual := (a[0] = b[0]) and (a[1] = b[1]) and (a[2] = b[2]) and (a[3] = b[3]);
end;

var
  f: Text;
  line: string;
  Before, After, Inp: TRegs;
  A, B, C: Integer;
  Op: Integer;
  Matches, Answer: Integer;
  Tmp: TRegs;

begin
  Assign(f, 'input.txt');
  Reset(f);
  Answer := 0;

  while not Eof(f) do begin
    Readln(f, line);
    if line = '' then continue;
    if Copy(line, 1, 6) = 'Before' then begin
      ParseLineTo4(line, Before);
      Readln(f, line);
      ParseLineTo4(line, Inp);
      A := Inp[1];
      B := Inp[2];
      C := Inp[3];
      Readln(f, line);
      ParseLineTo4(line, After);

      Matches := 0;
      for Op := 0 to 15 do begin
        Tmp := Before;
        ApplyOp(Op, A, B, C, Tmp);
        if RegsEqual(Tmp, After) then Inc(Matches);
      end;
      if Matches >= 3 then Inc(Answer);

      if not Eof(f) then Readln(f, line); // optional blank line
    end
    else if Copy(line, 1, 7) = 'Program' then begin
      Break;
    end;
  end;

  Close(f);
  WriteLn(Answer);
end.