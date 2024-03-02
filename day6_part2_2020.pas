program solution;

var
  inputFile: text;
  line: string;
  totalCount: integer;
  groupAnswers: array[Char] of integer;
  groupSize: integer;
  i: integer;
  c: Char;

begin
  assign(inputFile, 'input.txt');
  reset(inputFile);

  totalCount := 0;
  groupSize := 0;
  for i := 1 to 26 do
    groupAnswers[Chr(i + 96)] := 0;

  while not eof(inputFile) do
  begin
    readln(inputFile, line);
    if line = '' then
    begin
      for i := 1 to 26 do
      begin
        if groupAnswers[Chr(i + 96)] = groupSize then
          totalCount := totalCount + 1;
        groupAnswers[Chr(i + 96)] := 0;
      end;
      groupSize := 0;
    end
    else
    begin
      groupSize := groupSize + 1;
      for c in line do
        groupAnswers[c] := groupAnswers[c] + 1;
    end;
  end;

  for i := 1 to 26 do
  begin
    if groupAnswers[Chr(i + 96)] = groupSize then
      totalCount := totalCount + 1;
  end;

  writeln(totalCount);
  close(inputFile);
end.