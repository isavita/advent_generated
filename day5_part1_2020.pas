program Day5;

var
  inputFile: text;
  boardingPass: string;
  maxSeatID, currentSeatID, row, column: integer;

function calculateSeatID(row, column: integer): integer;
begin
  calculateSeatID := row * 8 + column;
end;

begin
  assign(inputFile, 'input.txt');
  reset(inputFile);

  maxSeatID := 0;

  while not eof(inputFile) do
  begin
    readln(inputFile, boardingPass);

    row := 0;
    column := 0;

    for i := 1 to 7 do
    begin
      if boardingPass[i] = 'B' then
        row := row * 2 + 1
      else
        row := row * 2;
    end;

    for i := 8 to 10 do
    begin
      if boardingPass[i] = 'R' then
        column := column * 2 + 1
      else
        column := column * 2;
    end;

    currentSeatID := calculateSeatID(row, column);

    if currentSeatID > maxSeatID then
      maxSeatID := currentSeatID;
  end;

  writeln(maxSeatID);

  close(inputFile);
end.