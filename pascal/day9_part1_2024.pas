
program DefragmentDisk;

{$MODE OBJFPC}{$H+} // Use Object Pascal mode and AnsiStrings

uses
  SysUtils; // Provides StrToInt, IntToStr, etc.

var
  InputFile: Text;
  Line: string;
  Disk: array of string; // Dynamic array to represent the disk
  C: Char;
  TotalSize, CurrentIndex, I, LengthVal, FileID: Integer;
  LFree, RFile: Integer;
  IsFile: Boolean;
  Checksum: Int64; // Use Int64 for potentially large checksum
  TempStr: string;

begin
  // Open input file
  Assign(InputFile, 'input.txt');
  Reset(InputFile);
  // Read the single line of input
  if not Eof(InputFile) then
    ReadLn(InputFile, Line)
  else
    Line := ''; // Handle empty file case
  Close(InputFile);

  // --- Optimized Initialization ---
  // Calculate total disk size first
  TotalSize := 0;
  for C in Line do
  begin
    // Input validation could be added here if needed
    TotalSize := TotalSize + (Ord(C) - Ord('0'));
  end;

  // Allocate the exact size for the disk array
  SetLength(Disk, TotalSize);

  // Fill the disk array in one pass
  CurrentIndex := 0;
  FileID := 0;
  IsFile := True;
  for C in Line do
  begin
    LengthVal := Ord(C) - Ord('0');
    if IsFile then
    begin
      TempStr := IntToStr(FileID);
      for I := 0 to LengthVal - 1 do
      begin
        if CurrentIndex < TotalSize then // Basic bounds check
        begin
          Disk[CurrentIndex] := TempStr;
          Inc(CurrentIndex);
        end;
      end;
      Inc(FileID);
    end
    else
    begin
      for I := 0 to LengthVal - 1 do
      begin
        if CurrentIndex < TotalSize then // Basic bounds check
        begin
          Disk[CurrentIndex] := '.';
          Inc(CurrentIndex);
        end;
      end;
    end;
    IsFile := not IsFile;
  end;
  // --- End Initialization ---

  // --- Defragmentation Process ---
  while True do
  begin
    // Find the leftmost free block index (LFree)
    LFree := -1;
    for I := 0 to Length(Disk) - 1 do
    begin
      if Disk[I] = '.' then
      begin
        LFree := I;
        Break; // Found the first free block, stop searching
      end;
    end;

    // If no free blocks left, disk is defragmented
    if LFree = -1 then Break;

    // Find the rightmost file block index (RFile) located after LFree
    RFile := -1;
    for I := Length(Disk) - 1 downto LFree + 1 do
    begin
      if Disk[I] <> '.' then
      begin
        RFile := I;
        Break; // Found the last file block, stop searching
      end;
    end;

    // If no file found to the right of the leftmost free block, we are done
    if RFile = -1 then Break;

    // Move the file from RFile to LFree
    Disk[LFree] := Disk[RFile];
    Disk[RFile] := '.'; // Mark the original position as free
  end;
  // --- End Defragmentation ---

  // --- Calculate Checksum ---
  Checksum := 0;
  for I := 0 to Length(Disk) - 1 do
  begin
    if Disk[I] <> '.' then
    begin
      // Multiply file ID (converted to integer) by its index
      Checksum := Checksum + Int64(I) * StrToInt(Disk[I]); // Cast index I to Int64 before multiplication
    end;
  end;
  // --- End Checksum Calculation ---

  // Print the final checksum
  WriteLn(Checksum);

end.
