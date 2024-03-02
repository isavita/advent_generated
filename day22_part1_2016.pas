program Solution;

uses
  SysUtils, Classes, RegExpr;

type
  Node = record
    used, avail: Integer;
  end;

var
  nodes: array of Node;
  nodeRegex: TRegExpr;
  filename: string;
  fileText: TStringList;
  i, j, used, avail, viablePairs: Integer;

begin
  SetLength(nodes, 0);
  
  filename := 'input.txt';
  fileText := TStringList.Create;
  fileText.LoadFromFile(filename);
  
  nodeRegex := TRegExpr.Create;
  nodeRegex.Expression := 'node-x\d+-y\d+\s+\d+T\s+(\d+)T\s+(\d+)T\s+\d+%';
  
  for i := 0 to fileText.Count - 1 do
  begin
    if nodeRegex.Exec(fileText[i]) then
    begin
      used := StrToInt(nodeRegex.Match[1]);
      avail := StrToInt(nodeRegex.Match[2]);
      SetLength(nodes, Length(nodes) + 1);
      nodes[High(nodes)].used := used;
      nodes[High(nodes)].avail := avail;
    end;
  end;
  
  viablePairs := 0;
  for i := 0 to High(nodes) do
  begin
    for j := 0 to High(nodes) do
    begin
      if (i <> j) and (nodes[i].used > 0) and (nodes[i].used <= nodes[j].avail) then
      begin
        Inc(viablePairs);
      end;
    end;
  end;
  
  writeln(viablePairs);
end.