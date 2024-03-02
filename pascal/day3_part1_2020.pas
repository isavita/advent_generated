program TobogganTrajectory;

var
  inputFile: TextFile;
  line: string;
  treesEncountered, position: integer;

begin
  AssignFile(inputFile, 'input.txt');
  Reset(inputFile);

  treesEncountered := 0;
  position := 1;

  while not Eof(inputFile) do
  begin
    ReadLn(inputFile, line);
    if line[position] = '#' then
      Inc(treesEncountered);
    
    position := (position + 3) mod Length(line);
    if position = 0 then
      position := Length(line);
  end;

  CloseFile(inputFile);

  WriteLn('Number of trees encountered: ', treesEncountered);
end.