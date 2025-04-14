
program Solve;

{$mode objfpc}{$H+} // Use Object Pascal mode and AnsiStrings

uses
  SysUtils;

// Function to increment the password string like a base-26 number
function IncrementPassword(password: string): string;
var
  i: Integer;
  ordVal: Integer;
begin
  Result := password; // Start with the original password
  for i := Length(Result) downto 1 do
  begin
    ordVal := Ord(Result[i]) + 1;
    if ordVal > Ord('z') then
    begin
      Result[i] := 'a'; // Wrap around
    end
    else
    begin
      Result[i] := Chr(ordVal); // Increment character
      Exit; // Exit function after successful increment
    end;
  end;
  // If we exit the loop, it means all chars wrapped around (e.g., 'zzzz' -> 'aaaa')
  // This case is unlikely given constraints but handled correctly.
end;

// Function to check for a straight of three increasing letters
function HasStraight(password: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  if Length(password) < 3 then Exit; // Cannot have a straight of 3

  for i := 1 to Length(password) - 2 do
  begin
    if (Ord(password[i]) + 1 = Ord(password[i+1])) and (Ord(password[i]) + 2 = Ord(password[i+2])) then
    begin
      Result := True;
      Exit; // Found one, no need to check further
    end;
  end;
end;

// Function to check if the password contains invalid letters ('i', 'o', 'l')
// Optimized using Pos function
function ContainsInvalidLetters(password: string): Boolean;
begin
  Result := (Pos('i', password) > 0) or (Pos('o', password) > 0) or (Pos('l', password) > 0);
end;

// Function to check for at least two different, non-overlapping pairs
function HasTwoPairs(password: string): Boolean;
var
  i, count: Integer;
begin
  count := 0;
  i := 1;
  while i < Length(password) do
  begin
    if password[i] = password[i+1] then
    begin
      Inc(count);
      Inc(i, 2); // Skip the pair
    end
    else
    begin
      Inc(i); // Move to the next character
    end;
  end;
  Result := (count >= 2);
end;

// Function to check if a password meets all criteria
function IsValidPassword(password: string): Boolean;
begin
  Result := HasStraight(password) and (not ContainsInvalidLetters(password)) and HasTwoPairs(password);
end;

// Function to find the next valid password starting from the given one
function FindNextPassword(password: string): string;
var
  currentPass: string;
begin
  currentPass := password;
  repeat
    currentPass := IncrementPassword(currentPass);
  until IsValidPassword(currentPass);
  Result := currentPass;
end;

// Main program entry point
var
  inputFile: TextFile;
  currentPassword: string;
  firstNewPassword: string;
  secondNewPassword: string;

begin
  // Assign and open the input file
  Assign(inputFile, 'input.txt');
  try
    Reset(inputFile);
    // Read the current password from the file
    if not Eof(inputFile) then
      ReadLn(inputFile, currentPassword)
    else
      begin
        WriteLn('Error: Input file is empty.');
        Halt(1); // Exit with error code
      end;
  except
    on E: EInOutError do
    begin
      WriteLn('Error reading input file: ', E.Message);
      Halt(1); // Exit with error code
    end;
  end;
  Close(inputFile);

  // Find the first valid password after the current one
  firstNewPassword := FindNextPassword(currentPassword);

  // Find the next valid password after the first new one
  secondNewPassword := FindNextPassword(firstNewPassword);

  // Print the second new password
  WriteLn(secondNewPassword);

end.
