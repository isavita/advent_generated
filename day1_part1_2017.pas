program InverseCaptcha;

{$mode objfpc}{$H+}

uses
  SysUtils;

var
  inputFile: TextFile;
  sequence: string;
  sum, i, currentDigit, nextDigit: Integer;

begin
  sum := 0;

  // Load the digit sequence from the input file
  AssignFile(inputFile, 'input.txt');
  Reset(inputFile);
  ReadLn(inputFile, sequence);
  CloseFile(inputFile);

  // Iterate through the digit sequence
  for i := 1 to Length(sequence) do
  begin
    // Convert current digit from char to integer
    currentDigit := StrToInt(sequence[i]);
    
    // Determine the next digit, wrapping around to the first if at the end
    if i < Length(sequence) then
      nextDigit := StrToInt(sequence[i + 1])
    else
      nextDigit := StrToInt(sequence[1]);
    
    // Add to sum if current digit matches the next digit
    if currentDigit = nextDigit then
      sum := sum + currentDigit;
  end;

  // Output the solution to the captcha
  WriteLn('The solution to the captcha is: ', sum);
end.
