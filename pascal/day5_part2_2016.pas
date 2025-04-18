
program EasterBunnyDoor5;
{$mode objfpc}{$H+}

uses
  SysUtils, md5;

const
  PASSWORD_LENGTH = 8;

var
  doorID : string;
  pwd1   : string;           // part 1 password
  pwd2   : string;           // part 2 password, we will mutate it
  filled : array[0..PASSWORD_LENGTH-1] of boolean;
  found2 : integer;          // how many positions we have filled in pwd2
  idx    : QWord;            // running index
  digest : TMD5Digest;
  hash   : string;
  pos    : integer;

begin
  // 1) Read Door ID from input.txt
  AssignFile(input, 'input.txt');
  Reset(input);
  ReadLn(doorID);
  CloseFile(input);

  // 2) Init
  pwd1 := '';
  SetLength(pwd2, PASSWORD_LENGTH);
  for pos := 0 to PASSWORD_LENGTH-1 do
    pwd2[pos+1] := '_';      // placeholder
  FillChar(filled, SizeOf(filled), False);
  found2 := 0;
  idx := 0;

  // 3) Search for qualifying hashes
  while (Length(pwd1) < PASSWORD_LENGTH) or (found2 < PASSWORD_LENGTH) do
  begin
    // compute MD5 of doorID + idx
    digest := MD5String(doorID + IntToStr(idx));
    hash := MD5Print(digest);

    // check for five leading zeros
    if Copy(hash,1,5) = '00000' then
    begin
      // PART 1: straightforward 6th char
      if Length(pwd1) < PASSWORD_LENGTH then
        pwd1 := pwd1 + hash[6];

      // PART 2: 6th char = position, 7th char = value
      pos := Ord(hash[6]) - Ord('0');
      if (pos in [0..PASSWORD_LENGTH-1]) and not filled[pos] then
      begin
        // fill that slot
        pwd2[pos+1] := hash[7];
        filled[pos] := True;
        Inc(found2);
      end;
    end;

    Inc(idx);
  end;

  // 4) Output
  WriteLn('Part 1 password: ', pwd1);
  WriteLn('Part 2 password: ', pwd2);
end.
