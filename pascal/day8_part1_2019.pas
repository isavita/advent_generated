program SpaceImageFormat;

{$APPTYPE CONSOLE}

const
  Width = 25;
  Height = 6;
  LayerSize = Width * Height;

var
  InputStr: ansistring;
  ch: char;
  LayerCount: Integer;
  MinZeros, BestOnes, BestTwos: Integer;
  Zeros, Ones, Twos: Integer;
  i, j, Offset: Integer;

begin
  { Redirect standard input to read from input.txt }
  Assign(Input, 'input.txt');
  Reset(Input);

  { Read all digit characters into a single string }
  InputStr := '';
  while not Eof(Input) do
  begin
    Read(Input, ch);
    if ch in ['0'..'9'] then
      InputStr := InputStr + ch;
  end;
  Close(Input);

  { Determine how many layers are in the image }
  LayerCount := Length(InputStr) div LayerSize;

  { Initialize the minimum zero count to a large value }
  MinZeros := MaxInt;

  { Examine each layer to find the one with the fewest '0' digits }
  for i := 0 to LayerCount - 1 do
  begin
    Zeros := 0;
    Ones := 0;
    Twos := 0;
    Offset := i * LayerSize;

    { Count 0s, 1s, and 2s in this layer }
    for j := 1 to LayerSize do
      case InputStr[Offset + j] of
        '0': Inc(Zeros);
        '1': Inc(Ones);
        '2': Inc(Twos);
      end;

    { If this layer has fewer zeros, remember its 1×2 product }
    if Zeros < MinZeros then
    begin
      MinZeros := Zeros;
      BestOnes := Ones;
      BestTwos := Twos;
    end;
  end;

  { Output the result: number of 1 digits multiplied by number of 2 digits }
  Writeln(BestOnes * BestTwos);
end.