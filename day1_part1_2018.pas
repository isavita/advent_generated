program Day1;

var
  inputFile: TextFile;
  frequencyChange, totalFrequency: Integer;

begin
  AssignFile(inputFile, 'input.txt');
  Reset(inputFile);
  
  totalFrequency := 0;
  
  while not Eof(inputFile) do
  begin
    Readln(inputFile, frequencyChange);
    totalFrequency := totalFrequency + frequencyChange;
  end;
  
  CloseFile(inputFile);
  
  WriteLn('Resulting frequency: ', totalFrequency);
end.