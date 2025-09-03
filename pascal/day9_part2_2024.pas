program FileDiskChecksum;

{$mode objfpc}{$H+}

type
  TFileSegment = record
    id: integer;
    start_pos: integer;
    end_pos: integer;
  end;

var
  f: TextFile;
  inputLine: string;
  totalSize, inputLen, numFiles, currentPos, fileIdCounter: integer;
  i, j, lengthSeg, fileLen, bestSpanStart, currentSpanLen: integer;
  disk: array of integer;
  files: array of TFileSegment;
  checksum: int64;

begin
  assign(f, 'input.txt');
  {$I-}
  reset(f);
  {$I+}
  if IOResult <> 0 then
    Halt(1);
  readln(f, inputLine);
  close(f);

  inputLen := Length(inputLine);
  totalSize := 0;
  for i := 1 to inputLen do
    if (inputLine[i] >= '0') and (inputLine[i] <= '9') then
      totalSize := totalSize + (Ord(inputLine[i]) - Ord('0'));

  if totalSize = 0 then
  begin
    writeln('0');
    Halt(0);
  end;

  SetLength(disk, totalSize);
  numFiles := (inputLen + 1) div 2;
  SetLength(files, numFiles);

  currentPos := 0;
  fileIdCounter := 0;

  for i := 1 to inputLen do
  begin
    lengthSeg := Ord(inputLine[i]) - Ord('0');
    if ((i - 1) mod 2 = 0) then
    begin
      files[fileIdCounter].id := fileIdCounter;
      files[fileIdCounter].start_pos := currentPos;
      files[fileIdCounter].end_pos := currentPos + lengthSeg - 1;
      for j := 0 to lengthSeg - 1 do
        disk[currentPos + j] := fileIdCounter;
      Inc(fileIdCounter);
    end
    else
    begin
      for j := 0 to lengthSeg - 1 do
        disk[currentPos + j] := -1;
    end;
    currentPos := currentPos + lengthSeg;
  end;

  for i := numFiles - 1 downto 0 do
  begin
    fileLen := files[i].end_pos - files[i].start_pos + 1;
    bestSpanStart := -1;
    currentSpanLen := 0;

    if files[i].start_pos > 0 then
      for j := 0 to files[i].start_pos - 1 do
      begin
        if disk[j] = -1 then
        begin
          if currentSpanLen = 0 then
            bestSpanStart := j;
          inc(currentSpanLen);
          if currentSpanLen = fileLen then
            break;
        end
        else
        begin
          currentSpanLen := 0;
          bestSpanStart := -1;
        end;
      end;

    if (bestSpanStart <> -1) and (currentSpanLen = fileLen) then
    begin
      for j := 0 to fileLen - 1 do
        disk[bestSpanStart + j] := files[i].id;
      for j := files[i].start_pos to files[i].end_pos do
        disk[j] := -1;
    end;
  end;

  checksum := 0;
  for i := 0 to totalSize - 1 do
  begin
    if disk[i] <> -1 then
      checksum := checksum + int64(i) * int64(disk[i]);
  end;

  writeln(checksum);
end.