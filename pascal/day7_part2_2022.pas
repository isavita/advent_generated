
program Day07NoSpaceLeftOnDevice;

{$mode objfpc}{$H+}
uses
  SysUtils;

type
  TFile = record
    name: string;
    size: Int64;
  end;

  PDir = ^TDir;
  TDir = record
    name: string;
    parent: PDir;
    subdirs: array of PDir;
    files: array of TFile;
  end;

  TInt64Array = array of Int64;

var
  root: PDir;
  current: PDir;

// Helpers
function FindSubdir(parent: PDir; name: string): PDir;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to High(parent^.subdirs) do
    if parent^.subdirs[i]^.name = name then
    begin
      Result := parent^.subdirs[i];
      Exit;
    end;
end;

procedure AddSubdir(parent: PDir; name: string; out child: PDir);
var
  nd: PDir;
begin
  New(nd);
  nd^.name := name;
  nd^.parent := parent;
  SetLength(nd^.subdirs, 0);
  SetLength(nd^.files, 0);
  SetLength(parent^.subdirs, Length(parent^.subdirs) + 1);
  parent^.subdirs[Length(parent^.subdirs) - 1] := nd;
  child := nd;
end;

procedure EnsureSubdir(parent: PDir; name: string; out child: PDir);
var
  f: PDir;
begin
  f := FindSubdir(parent, name);
  if f = nil then
    AddSubdir(parent, name, child)
  else
    child := f;
end;

procedure AddFile(dir: PDir; name: string; size: Int64);
begin
  SetLength(dir^.files, Length(dir^.files) + 1);
  dir^.files[Length(dir^.files) - 1].name := name;
  dir^.files[Length(dir^.files) - 1].size := size;
end;

// Returns total size of directory d. Appends every directory's total size into 'sizes'
// and accumulates the sum of directories with total size <= 100000 into sumAtMost.
function DirTotalSize(d: PDir; var sizes: TInt64Array; var sumAtMost: Int64): Int64;
var
  s: Int64;
  i: Integer;
begin
  s := 0;
  for i := 0 to High(d^.files) do
    s := s + d^.files[i].size;
  for i := 0 to High(d^.subdirs) do
    s := s + DirTotalSize(d^.subdirs[i], sizes, sumAtMost);

  SetLength(sizes, Length(sizes) + 1);
  sizes[Length(sizes) - 1] := s;

  if s <= 100000 then
    sumAtMost := sumAtMost + s;

  DirTotalSize := s;
end;

var
  f: Text;
  line: string;
  arg: string;
  ch: PDir;
  dname: string;
  posSpace: Integer;
  sizeStr, nameToken: string;
  rootSize: Int64;
  sumAtMost: Int64;
  sizes: TInt64Array;
  totalDisk: Int64;
  needSpace: Int64;
  freeSpace: Int64;
  need: Int64;
  best: Int64;
  i: Integer;

begin
  // Initialize root "/"
  New(root);
  root^.name := '/';
  root^.parent := nil;
  SetLength(root^.subdirs, 0);
  SetLength(root^.files, 0);
  current := root;

  // Read input from input.txt
  if not FileExists('input.txt') then
  begin
    WriteLn('0');
    Exit;
  end;

  Assign(f, 'input.txt');
  Reset(f);
  while not Eof(f) do
  begin
    ReadLn(f, line);
    line := Trim(line);
    if line = '' then Continue;

    if Copy(line, 1, 4) = '$ ls' then
    begin
      // nothing to do; contents follow
      Continue;
    end
    else if Copy(line, 1, 5) = '$ cd ' then
    begin
      arg := Copy(line, 6, Length(line) - 5);
      if arg = '/' then
        current := root
      else if arg = '..' then
        current := current^.parent
      else
      begin
        ch := FindSubdir(current, arg);
        if ch = nil then
          AddSubdir(current, arg, ch);
        current := ch;
      end;
    end
    else
    begin
      // ls contents
      if Copy(line, 1, 4) = 'dir ' then
      begin
        dname := Copy(line, 5, Length(line) - 4);
        ch := FindSubdir(current, dname);
        if ch = nil then AddSubdir(current, dname, ch);
      end
      else
      begin
        posSpace := Pos(' ', line);
        sizeStr := Copy(line, 1, posSpace - 1);
        nameToken := Copy(line, posSpace + 1, Length(line) - posSpace);
        AddFile(current, nameToken, StrToInt64(sizeStr));
      end;
    end;
  end;
  Close(f);

  // Compute sizes
  SetLength(sizes, 0);
  sumAtMost := 0;
  rootSize := DirTotalSize(root, sizes, sumAtMost);

  // Part 1 answer
  WriteLn(sumAtMost);

  // Part 2: find smallest directory that, if deleted, frees enough space
  totalDisk := 70000000;
  needSpace := 30000000;
  freeSpace := totalDisk - rootSize;
  need := needSpace - freeSpace;
  if need < 0 then need := 0;

  best := High(Int64);
  for i := 0 to High(sizes) do
  begin
    if (sizes[i] >= need) and (sizes[i] < best) then
      best := sizes[i];
  end;

  WriteLn(best);
end.
