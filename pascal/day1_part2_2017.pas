program InverseCaptchaPart2;

{$mode objfpc}{$H+}

uses
  SysUtils;

var
  inputFile: TextFile;
  sequence: string;
  sum, i, currentDigit, compareDigit, halfLength, compareIndex: Integer;

begin
  sum := 0;

  // Load the digit sequence from the input file
  AssignFile(inputFile, 'input.txt');
  Reset(inputFile);
  ReadLn(inputFile, sequence);
  CloseFile(inputFile);

  // Calculate half of the sequence length
  halfLength := Length(sequence) div 2;

  // Iterate through the digit sequence
  for i := 1 to Length(sequence) do
  begin
    // Convert current digit from char to integer
    currentDigit := StrToInt(sequence[i]);
    
    // Calculate the index of the digit to compare, wrapping around the circular list
    compareIndex := i + halfLength;
    if compareIndex > Length(sequence) then
      compareIndex := compareIndex - Length(sequence);
    
    // Convert the compare digit from char to integer
    compareDigit := StrToInt(sequence[compareIndex]);
    
    // Add to sum if current digit matches the compare digit
    if currentDigit = compareDigit then
      sum := sum + currentDigit;
  end;

  // Output the solution to the captcha
  WriteLn('The solution to the new captcha is: ', sum);
end.
