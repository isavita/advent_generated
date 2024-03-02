program Day5;

uses md5;

var
  InputFile: Text;
  DoorID: string;
  Index: Integer;
  Hash: string;
  Password: string;

begin
  Assign(InputFile, 'input.txt');
  Reset(InputFile);
  Readln(InputFile, DoorID);
  Close(InputFile);

  Index := 0;
  Password := '';

  repeat
    Hash := MD5Print(MD5String(DoorID + IntToStr(Index)));
    Inc(Index);
    if (Hash[1] = '0') and (Hash[2] = '0') and (Hash[3] = '0') and (Hash[4] = '0') and (Hash[5] = '0') then
      Password := Password + Hash[6];
  until Length(Password) = 8;

  WriteLn(Password);
end.