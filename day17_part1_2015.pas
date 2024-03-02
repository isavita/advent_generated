program Solution;

uses SysUtils, Classes;

function countCombinations(containers: array of integer; target, index: integer): integer;
begin
  if target = 0 then
    countCombinations := 1
  else if (target < 0) or (index >= Length(containers)) then
    countCombinations := 0
  else
    countCombinations := countCombinations(containers, target - containers[index], index + 1) +
                        countCombinations(containers, target, index + 1);
end;

var
  fileInput: TextFile;
  containers: array of integer;
  size, i: integer;
begin
  AssignFile(fileInput, 'input.txt');
  Reset(fileInput);

  SetLength(containers, 0);
  while not Eof(fileInput) do
  begin
    ReadLn(fileInput, size);
    SetLength(containers, Length(containers) + 1);
    containers[High(containers)] := size;
  end;

  CloseFile(fileInput);

  Writeln(countCombinations(containers, 150, 0));
end.