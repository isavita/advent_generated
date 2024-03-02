program Solution;

uses SysUtils, Classes;

var
  inputFile: Text;
  strArr: TStringList;
  banks: array of Integer;
  seen: TStringList;
  cycles, i, maxIndex, blocks: Integer;
  state: String;

begin
  // Step 1: Read Input
  Assign(inputFile, 'input.txt');
  Reset(inputFile);
  strArr := TStringList.Create;
  while not Eof(inputFile) do
  begin
    Readln(inputFile, state);
    strArr.DelimitedText := state;
  end;
  Close(inputFile);

  SetLength(banks, strArr.Count);
  for i := 0 to strArr.Count - 1 do
  begin
    banks[i] := StrToInt(strArr[i]);
  end;

  // Step 2: Initialize Variables
  seen := TStringList.Create;
  cycles := 0;

  // Step 3: Redistribution Loop
  repeat
    // Convert current banks state to string to store in set
    state := '';
    for i := 0 to Length(banks) - 1 do
    begin
      state := state + IntToStr(banks[i]) + ' ';
    end;

    // Step 4: Check for Repeats
    if seen.IndexOf(state) > -1 then
    begin
      Break;
    end;
    seen.Add(state);

    // Find the bank with most blocks
    maxIndex := 0;
    for i := 1 to Length(banks) - 1 do
    begin
      if banks[i] > banks[maxIndex] then
      begin
        maxIndex := i;
      end;
    end;

    // Perform redistribution
    blocks := banks[maxIndex];
    banks[maxIndex] := 0;
    for i := 1 to blocks do
    begin
      banks[(maxIndex + i) mod Length(banks)] := banks[(maxIndex + i) mod Length(banks)] + 1;
    end;

    // Increment cycle counter
    cycles := cycles + 1;
  until False;

  // Output
  Writeln('It takes ', cycles, ' redistribution cycles to reach a repeated configuration.');
end.