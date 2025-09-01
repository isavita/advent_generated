
program LongestPath;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, md5;

type
  TNode = record
    x, y: Integer;
    path: string;
  end;

var
  passcode, line: string;
  queue: array of TNode;
  head, longest, i, nx, ny: Integer;
  dirs: string = 'UDLR';
  dx: array[0..3] of Integer = (0, 0, -1, 1);
  dy: array[0..3] of Integer = (-1, 1, 0, 0);
  hash, digest: string;
  node, nextNode: TNode;

begin
  AssignFile(Input, 'input.txt');
  Reset(Input);
  ReadLn(Input, line);
  CloseFile(Input);
  passcode := Trim(line);

  SetLength(queue, 1);
  queue[0].x := 0;
  queue[0].y := 0;
  queue[0].path := '';
  head := 0;
  longest := 0;

  while head < Length(queue) do
  begin
    node := queue[head];
    Inc(head);

    if (node.x = 3) and (node.y = 3) then
    begin
      if Length(node.path) > longest then
        longest := Length(node.path);
      Continue;
    end;

    hash := MD5Print(MD5String(passcode + node.path));

    for i := 0 to 3 do
      if (hash[i + 1] in ['b'..'f']) then
      begin
        nx := node.x + dx[i];
        ny := node.y + dy[i];
        if (nx >= 0) and (nx < 4) and (ny >= 0) and (ny < 4) then
        begin
          nextNode.x := nx;
          nextNode.y := ny;
          nextNode.path := node.path + dirs[i + 1];
          SetLength(queue, Length(queue) + 1);
          queue[High(queue)] := nextNode;
        end;
      end;
  end;

  WriteLn(longest);
end.
