Program DragonChecksum;

{ 
  This program reads an initial state (a binary string of 0s and 1s)
  from "input.txt", then expands it using the modified dragon curve
  until it reaches at least 272 characters. It truncates to exactly
  272 characters, computes the checksum by pairing characters and
  reducing until an odd‐length checksum remains, and writes the result
  to standard output.
}

{$mode objfpc}{$H+}

uses
  SysUtils;

const
  DiskLength = 272;

var
  f       : Text;
  s, b    : string;
  checksum: string;
  i, pairs: Integer;
  
begin
  { Read the initial state from input.txt }
  Assign(f, 'input.txt');
  Reset(f);
  ReadLn(f, s);
  Close(f);

  { Expand via dragon curve until length ≥ DiskLength }
  while Length(s) < DiskLength do
  begin
    b := '';
    { Build the reversed & flipped copy }
    for i := Length(s) downto 1 do
      if s[i] = '0' then
        b := b + '1'
      else
        b := b + '0';
    s := s + '0' + b;
  end;

  { Truncate to exactly DiskLength }
  if Length(s) > DiskLength then
    s := Copy(s, 1, DiskLength);

  { Compute checksum until its length is odd }
  while (Length(s) mod 2) = 0 do
  begin
    checksum := '';
    pairs := Length(s) div 2;
    for i := 1 to pairs do
      if s[2*i - 1] = s[2*i] then
        checksum := checksum + '1'
      else
        checksum := checksum + '0';
    s := checksum;
  end;

  { Output the final checksum }
  WriteLn(s);
end.