
program XMASDecoder;
{$mode objfpc}{$h+}

uses
  SysUtils;

const
  PREAMBLE = 25;

var
  f     : TextFile;
  nums  : array of Int64;
  i, j, k: Integer;
  current: Int64;
  valid : Boolean;

begin
  {--- Read all numbers from input.txt into dynamic array ---}
  AssignFile(f, 'input.txt');
  Reset(f);
  while not Eof(f) do
  begin
    SetLength(nums, Length(nums) + 1);
    ReadLn(f, nums[High(nums)]);
  end;
  CloseFile(f);

  {--- For each number after the preamble, check if it is the sum of two of the previous PREAMBLE numbers ---}
  for i := PREAMBLE to High(nums) do
  begin
    current := nums[i];
    valid := False;

    { scan each pair in the window [i-PREAMBLE .. i-1] }
    for j := i - PREAMBLE to i - 1 do
    begin
      for k := j + 1 to i - 1 do
      begin
        if nums[j] + nums[k] = current then
        begin
          valid := True;
          Break;
        end;
      end;
      if valid then
        Break;
    end;

    { as soon as we find an invalid number, print and exit }
    if not valid then
    begin
      WriteLn(current);
      Exit;
    end;
  end;
end.
