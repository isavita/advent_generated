
{$MODE OBJFPC}{$H+} // Use Object Pascal mode and AnsiStrings

program LookAndSaySolver;

uses
  SysUtils; // Provides IntToStr, TStringBuilder, file handling routines

// Function to perform one iteration of the Look-and-Say sequence
function LookAndSay(inputStr: string): string;
var
  sb: TStringBuilder; // Use StringBuilder for efficient string construction
  i, count: Integer;
  currentChar: Char;
begin
  if Length(inputStr) = 0 then
  begin
    Result := ''; // Handle empty input
    Exit;
  end;

  sb := TStringBuilder.Create;
  try
    i := 1; // Pascal strings are 1-based
    while i <= Length(inputStr) do
    begin
      currentChar := inputStr[i];
      count := 1;
      // Count consecutive identical characters
      while (i + 1 <= Length(inputStr)) and (inputStr[i + 1] = currentChar) do
      begin
        Inc(i);
        Inc(count);
      end;
      // Append the count and the character
      sb.Append(IntToStr(count));
      sb.Append(currentChar);
      Inc(i); // Move to the next character group
    end;
    Result := sb.ToString; // Assign the final string to the function result
  finally
    sb.Free; // Ensure StringBuilder is freed
  end;
end;

// Main program entry point
var
  inputFile: TextFile;
  data: string;
  i: Integer;

begin
  // Open the input file
  Assign(inputFile, 'input.txt');
  try
    Reset(inputFile);
    // Read the entire first line (assuming input is on one line)
    if not Eof(inputFile) then
      Readln(inputFile, data)
    else
      data := ''; // Handle empty file
  except
    on E: EInOutError do
    begin
      WriteLn('Error reading input file: ', E.Message);
      Halt(1); // Exit with error code
    end;
  end;
  Close(inputFile);

  // Apply the Look-and-Say process 40 times
  for i := 1 to 40 do
  begin
    data := LookAndSay(data);
  end;

  // Print the length of the final result
  WriteLn(Length(data));

end.
