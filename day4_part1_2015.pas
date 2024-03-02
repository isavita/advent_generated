program Day4Part1;

{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}

uses
  SysUtils, Classes, md5;

var
  secretKey: String;
  num: Integer;
  hashResult: String;
  inputFile: TextFile;

function MD5StartsWithFiveZeroes(const hash: String): Boolean;
begin
  // Check if the first five characters of the MD5 hash are zeroes
  Result := Pos('00000', hash) = 1;
end;

begin
  // Load the secret key from file
  AssignFile(inputFile, 'input.txt');
  Reset(inputFile);
  ReadLn(inputFile, secretKey);
  CloseFile(inputFile);

  num := 0;
  repeat
    Inc(num);
    // Compute MD5 hash of the secret key concatenated with the current number
    hashResult := MD5Print(MD5String(secretKey + IntToStr(num)));
  until MD5StartsWithFiveZeroes(hashResult);

  // Output the first number that results in a hash starting with at least five zeroes
  WriteLn('The lowest positive number: ', num);
end.
