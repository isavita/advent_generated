
program SpreadsheetChecksum;

var
  inputFile: TextFile;
  checksum: LongInt;
  currentNum, rowMin, rowMax: Integer;
  firstNum: Boolean;

begin
  Assign(inputFile, 'input.txt');
  Reset(inputFile);

  checksum := 0;

  while not EOF(inputFile) do
  begin
    // Check if the line is not empty before processing
    if not EOLn(inputFile) then
    begin
      // Read the first number to initialize min/max for the row
      Read(inputFile, currentNum);
      rowMin := currentNum;
      rowMax := currentNum;

      // Read remaining numbers on the line
      while not EOLn(inputFile) do
      begin
        Read(inputFile, currentNum);
        if currentNum < rowMin then
          rowMin := currentNum;
        if currentNum > rowMax then
          rowMax := currentNum;
      end;

      // Add the difference for this row to the total checksum
      checksum := checksum + (rowMax - rowMin);
    end;

    // Move to the next line (handles empty lines too)
    ReadLn(inputFile);
  end;

  Close(inputFile);

  WriteLn(checksum);
end.
