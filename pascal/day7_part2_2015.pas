program Circuit;
uses SysUtils, Classes;
type
  TRule = record
    op: Integer; a, b, dest: string; val: Cardinal; done: Boolean;
  end;
var
  rules: array of TRule;
  n, idxB: Integer;
  sl: TStringList;

function gv(s: string): Cardinal;
var i: Integer; res: Cardinal;
begin
  if (s <> '') and (s[1] in ['0'..'9']) then Exit(StrToInt(s));
  for i := 0 to n - 1 do
    if rules[i].dest = s then
    begin
      if rules[i].done then Exit(rules[i].val);
      case rules[i].op of
        0: res := gv(rules[i].a);
        1: res := (not gv(rules[i].b)) and $FFFF;
        2: res := gv(rules[i].a) and gv(rules[i].b);
        3: res := gv(rules[i].a) or gv(rules[i].b);
        4: res := gv(rules[i].a) shl gv(rules[i].b);
        5: res := gv(rules[i].a) shr gv(rules[i].b);
      end;
      rules[i].val := res;
      rules[i].done := True;
      Exit(res);
    end;
  Exit(0);
end;

var
  i, p, p2: Integer;
  line, lhs, wd, w1, w2: string;
  op: Integer;
  a1, a2: Cardinal;
begin
  sl := TStringList.Create;
  sl.LoadFromFile('input.txt');
  n := sl.Count;
  SetLength(rules, n);
  for i := 0 to n - 1 do
  begin
    line := Trim(sl[i]);
    p := Pos('->', line);
    lhs := Trim(Copy(line, 1, p - 1));
    wd := Trim(Copy(line, p + 2, MaxInt));
    op := 0; w1 := ''; w2 := '';
    if Pos('AND', lhs) > 0 then
    begin
      op := 2;
      p2 := Pos('AND', lhs);
      w1 := Trim(Copy(lhs, 1, p2 - 1));
      w2 := Trim(Copy(lhs, p2 + 3, MaxInt));
    end
    else if Pos('OR', lhs) > 0 then
    begin
      op := 3;
      p2 := Pos('OR', lhs);
      w1 := Trim(Copy(lhs, 1, p2 - 1));
      w2 := Trim(Copy(lhs, p2 + 2, MaxInt));
    end
    else if Pos('LSHIFT', lhs) > 0 then
    begin
      op := 4;
      p2 := Pos('LSHIFT', lhs);
      w1 := Trim(Copy(lhs, 1, p2 - 1));
      w2 := Trim(Copy(lhs, p2 + 6, MaxInt));
    end
    else if Pos('RSHIFT', lhs) > 0 then
    begin
      op := 5;
      p2 := Pos('RSHIFT', lhs);
      w1 := Trim(Copy(lhs, 1, p2 - 1));
      w2 := Trim(Copy(lhs, p2 + 6, MaxInt));
    end
    else if Pos('NOT', lhs) = 1 then
    begin
      op := 1;
      w2 := Trim(Copy(lhs, 4, MaxInt));
    end
    else
    begin
      op := 0;
      w1 := lhs;
    end;
    rules[i].op := op;
    rules[i].a := w1;
    rules[i].b := w2;
    rules[i].dest := wd;
    rules[i].done := False;
    if wd = 'b' then idxB := i;
  end;
  a1 := gv('a');
  for i := 0 to n - 1 do rules[i].done := False;
  rules[idxB].op := 0;
  rules[idxB].a := IntToStr(a1);
  for i := 0 to n - 1 do rules[i].done := False;
  a2 := gv('a');
  WriteLn(a2);
end.