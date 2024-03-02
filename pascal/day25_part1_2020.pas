program Day25;

var
  cardPublicKey, doorPublicKey, cardLoopSize, doorLoopSize, value, subjectNumber, encryptionKey: Int64;

begin
  AssignFile(input, 'input.txt');
  Reset(input);

  ReadLn(cardPublicKey);
  ReadLn(doorPublicKey);

  subjectNumber := 7;
  value := 1;
  cardLoopSize := 0;
  while value <> cardPublicKey do
  begin
    Inc(cardLoopSize);
    value := (value * subjectNumber) mod 20201227;
  end;

  value := 1;
  for doorLoopSize := 1 to cardLoopSize do
    value := (value * doorPublicKey) mod 20201227;

  encryptionKey := value;

  CloseFile(input);

  WriteLn('Encryption key: ', encryptionKey);
end.