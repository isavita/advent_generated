program solution;

uses SysUtils;

procedure findCombinations(containers: array of integer; target, index, count: integer; var minCount, ways: integer);
begin
  if target = 0 then
  begin
    if (minCount = 0) or (count < minCount) then
    begin
      minCount := count;
      ways := 1;
    end
    else if count = minCount then
    begin
      ways := ways + 1;
    end;
    Exit;
  end;
  if (target < 0) or (index >= Length(containers)) then
  begin
    Exit;
  end;
  findCombinations(containers, target - containers[index], index + 1, count + 1, minCount, ways);
  findCombinations(containers, target, index + 1, count, minCount, ways);
end;

var
  fileInput: Text;
  containers: array of integer;
  size, minCount, ways, i: integer;
begin
  Assign(fileInput, 'input.txt');
  Reset(fileInput);

  i := 0;
  while not Eof(fileInput) do
  begin
    Readln(fileInput, size);
    SetLength(containers, i + 1);
    containers[i] := size;
    i := i + 1;
  end;

  Close(fileInput);

  minCount := 0;
  ways := 0;
  findCombinations(containers, 150, 0, 0, minCount, ways);
  Writeln(ways);
end.