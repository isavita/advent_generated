program ReportRepair;

var
  input: text;
  expenses: array of Integer;
  expense: Integer;
  i, j: Integer;

begin
  Assign(input, 'input.txt');
  Reset(input);

  SetLength(expenses, 0);
  while not EOF(input) do
  begin
    SetLength(expenses, Length(expenses) + 1);
    ReadLn(input, expenses[High(expenses)]);
  end;

  Close(input);

  for i := 0 to High(expenses) do
  begin
    for j := i + 1 to High(expenses) do
    begin
      if expenses[i] + expenses[j] = 2020 then
      begin
        WriteLn(expenses[i] * expenses[j]);
        Exit;
      end;
    end;
  end;
end.