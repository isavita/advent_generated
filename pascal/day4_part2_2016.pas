
program SolveRooms;

uses SysUtils;

var
  InputFile: Text;
  Line: String;
  NamePart, SectorIdStr, ChecksumPart: String;
  SectorId: Integer;
  Checksum: String;
  TotalSectorIds: LongInt;
  I, J: Integer;

type
  TCharPair = record
    Ch: Char;
    Count: Integer;
  end;

function IsRealRoom(Name: String; Checksum: String): Boolean;
var
  NameClean: String;
  I, J: Integer;
  Counts: array[0..25] of Integer;
  CharPairs: array[0..25] of TCharPair;
  TempPair: TCharPair;
  ChecksumCalculated: String;
begin
  NameClean := '';
  for I := 1 to Length(Name) do
    if Name[I] <> '-' then
      NameClean := NameClean + Name[I];

  for I := 0 to 25 do
    Counts[I] := 0;
  for I := 1 to Length(NameClean) do
    Inc(Counts[Ord(NameClean[I]) - Ord('a')]);

  for I := 0 to 25 do
  begin
    CharPairs[I].Ch := Chr(Ord('a') + I);
    CharPairs[I].Count := Counts[I];
  end;

  for I := 0 to 24 do
    for J := I + 1 to 25 do
      if (CharPairs[J].Count > CharPairs[I].Count) or
         ((CharPairs[J].Count = CharPairs[I].Count) and (CharPairs[J].Ch < CharPairs[I].Ch)) then
      begin
        TempPair := CharPairs[I];
        CharPairs[I] := CharPairs[J];
        CharPairs[J] := TempPair;
      end;

  ChecksumCalculated := '';
  for I := 0 to 4 do
    ChecksumCalculated := ChecksumCalculated + CharPairs[I].Ch;

  IsRealRoom := ChecksumCalculated = Checksum;
end;

function DecryptName(Name: String; SectorId: Integer): String;
var
  Decrypted: String;
  I: Integer;
  Shift: Integer;
  Base: Integer;
begin
  Decrypted := '';
  Shift := SectorId mod 26;
  Base := Ord('a');

  for I := 1 to Length(Name) do
    if Name[I] = '-' then
      Decrypted := Decrypted + ' '
    else
      Decrypted := Decrypted + Chr(((Ord(Name[I]) - Base + Shift) mod 26) + Base);
  DecryptName := Decrypted;
end;

begin
  TotalSectorIds := 0;

  Assign(InputFile, 'input.txt');
  {$I-}
  Reset(InputFile);
  {$I+}

  if IOResult <> 0 then
    Exit;

  while not Eof(InputFile) do
  begin
    ReadLn(InputFile, Line);

    I := Length(Line);
    while (I > 0) and (Line[I] <> '-') do
      Dec(I);

    NamePart := Copy(Line, 1, I - 1);

    SectorIdStr := Copy(Line, I + 1, Length(Line) - I);

    J := Pos('[', SectorIdStr);
    ChecksumPart := Copy(SectorIdStr, J, Length(SectorIdStr) - J + 1);
    SectorIdStr := Copy(SectorIdStr, 1, J - 1);

    Checksum := Copy(ChecksumPart, 2, Length(ChecksumPart) - 2);

    Val(SectorIdStr, SectorId, J);

    if J <> 0 then
      Continue;

    if IsRealRoom(NamePart, Checksum) then
    begin
      TotalSectorIds := TotalSectorIds + SectorId;

      if DecryptName(NamePart, SectorId) = 'northpole object storage' then
        Writeln(SectorId);
    end;
  end;

  Close(InputFile);

  Writeln(TotalSectorIds);

end.
