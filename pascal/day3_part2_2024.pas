
program MullItOver;
{$mode objfpc}{$H+}

var
  fullText, line: string;
  i, j, lengthText, digitCount: Integer;
  enabled: Boolean;
  resultSum, x, y, v: Int64;

begin
  {--- Read entire input.txt into one string (preserve newlines so we
     never spuriously join instruction fragments across lines) ---}
  Assign(Input,'input.txt');
  Reset(Input);
  fullText := '';
  while not Eof(Input) do
  begin
    ReadLn(line);
    fullText := fullText + line + #10;
  end;
  Close(Input);

  lengthText := Length(fullText);
  enabled := True;
  resultSum := 0;
  i := 1;

  {--- Single‐pass scan ---}
  while i <= lengthText do
  begin
    {--- Check for "don't()" (7 chars) ---}
    if (i + 6 <= lengthText)
       and (fullText[i]   = 'd')
       and (fullText[i+1] = 'o')
       and (fullText[i+2] = 'n')
       and (fullText[i+3] = #39)     { apostrophe }
       and (fullText[i+4] = 't')
       and (fullText[i+5] = '(')
       and (fullText[i+6] = ')') then
    begin
      enabled := False;
      Inc(i,7);
      Continue;
    end
    {--- Check for "do()" (4 chars) ---}
    else if (i + 3 <= lengthText)
         and (fullText[i]   = 'd')
         and (fullText[i+1] = 'o')
         and (fullText[i+2] = '(')
         and (fullText[i+3] = ')') then
    begin
      enabled := True;
      Inc(i,4);
      Continue;
    end
    {--- Check for "mul(" and attempt to parse mul(X,Y) ---}
    else if (i + 3 <= lengthText)
         and (fullText[i]   = 'm')
         and (fullText[i+1] = 'u')
         and (fullText[i+2] = 'l')
         and (fullText[i+3] = '(') then
    begin
      { parse X (1–3 digits) }
      v := 0; digitCount := 0; j := i + 4;
      while (j <= lengthText)
            and (fullText[j] in ['0'..'9'])
            and (digitCount < 3) do
      begin
        v := v * 10 + (Ord(fullText[j]) - Ord('0'));
        Inc(digitCount);
        Inc(j);
      end;
      { must have at least 1 digit, then a comma }
      if (digitCount >= 1) and (j <= lengthText) and (fullText[j] = ',') then
      begin
        x := v;
        { parse Y (1–3 digits) }
        v := 0; digitCount := 0;
        Inc(j);  { skip comma }
        while (j <= lengthText)
              and (fullText[j] in ['0'..'9'])
              and (digitCount < 3) do
        begin
          v := v * 10 + (Ord(fullText[j]) - Ord('0'));
          Inc(digitCount);
          Inc(j);
        end;
        { must have at least 1 digit, then a closing parenthesis }
        if (digitCount >= 1) and (j <= lengthText) and (fullText[j] = ')') then
        begin
          y := v;
          if enabled then
            resultSum := resultSum + x * y;
          { advance past the ')' }
          i := j + 1;
          Continue;
        end;
      end;
    end;

    { otherwise skip this character and continue }
    Inc(i);
  end;

  {--- Output the final sum of enabled mul(...) results ---}
  WriteLn(resultSum);
end.
