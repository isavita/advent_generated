program Solution;

uses SysUtils, Classes;

var
  inputFile: Text;
  line, numStr: string;
  nums: TStrings;
  num, sum, i, j: Integer;

begin
  Assign(inputFile, 'input.txt');
  Reset(inputFile);

  sum := 0;

  while not Eof(inputFile) do
  begin
    ReadLn(inputFile, line);
    nums := TStringList.Create;
    nums.Delimiter := ' ';
    nums.DelimitedText := line;

    for i := 0 to nums.Count - 1 do
    begin
      numStr := nums[i];
      num := StrToInt(numStr);

      for j := 0 to nums.Count - 1 do
      begin
        if (i <> j) and (num mod StrToInt(nums[j]) = 0) then
        begin
          sum := sum + (num div StrToInt(nums[j]));
        end;
      end;
    end;

    nums.Free;
  end;

  Close(inputFile);

  WriteLn(sum);
end.