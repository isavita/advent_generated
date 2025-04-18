
program MullItOver;

{$mode objfpc}{$h+}

uses
  SysUtils;

var
  F: TextFile;
  Line, S: string;
  i, N, Sum, Num1, Num2, L1, L2, J: Integer;
begin
  // 1) Read entire file into one string S
  AssignFile(F, 'input.txt');
  Reset(F);
  S := '';
  while not Eof(F) do
  begin
    ReadLn(F, Line);
    S += Line;
  end;
  CloseFile(F);

  // 2) Scan for "mul(" then parse up to 3 digits, a comma, up to 3 digits, and a ')'
  Sum := 0;
  N := Length(S);
  i := 1;
  while i <= N - 4 do
  begin
    if (S[i] = 'm') and (S[i+1] = 'u') and (S[i+2] = 'l') and (S[i+3] = '(') then
    begin
      // parse first number (1–3 digits)
      J := i + 4;
      Num1 := 0; L1 := 0;
      while (J <= N) and (L1 < 3) and (S[J] in ['0'..'9']) do
      begin
        Num1 := Num1 * 10 + (Ord(S[J]) - Ord('0'));
        Inc(L1); Inc(J);
      end;
      // must have at least one digit and a comma next
      if (L1 >= 1) and (J <= N) and (S[J] = ',') then
      begin
        Inc(J);
        // parse second number (1–3 digits)
        Num2 := 0; L2 := 0;
        while (J <= N) and (L2 < 3) and (S[J] in ['0'..'9']) do
        begin
          Num2 := Num2 * 10 + (Ord(S[J]) - Ord('0'));
          Inc(L2); Inc(J);
        end;
        // must have at least one digit and a closing parenthesis
        if (L2 >= 1) and (J <= N) and (S[J] = ')') then
        begin
          Sum := Sum + Num1 * Num2;
          // advance i past this ')' to avoid re‐parsing inside the same instruction
          i := J;
        end;
      end;
    end;
    Inc(i);
  end;

  // 3) Output the result
  WriteLn(Sum);
end.
