program Solution;
var
  data: array of Integer;
  n, pos, x: Integer;
  total, rootValue: Int64;

procedure parseNode(var pos: Integer; var total, value: Int64);
var
  numC, numM, i, j: Integer;
  vals, md: array of Integer;
  cT, cV: Int64;
begin
  numC := data[pos];
  numM := data[pos+1];
  pos := pos + 2;
  SetLength(vals, numC);
  for i := 0 to numC-1 do
  begin
    parseNode(pos, cT, cV);
    total := total + cT;
    vals[i] := cV;
  end;
  SetLength(md, numM);
  for i := 0 to numM-1 do
  begin
    md[i] := data[pos];
    total := total + md[i];
    pos := pos + 1;
  end;
  if numC = 0 then
  begin
    value := 0;
    for i := 0 to numM-1 do
      value := value + md[i];
  end
  else
  begin
    value := 0;
    for i := 0 to numM-1 do
    begin
      j := md[i];
      if (j > 0) and (j <= numC) then
        value := value + vals[j-1];
    end;
  end;
end;

begin
  Assign(input, 'input.txt');
  Reset(input);
  n := 0;
  while not Eof do
  begin
    Read(input, x);
    SetLength(data, n+1);
    data[n] := x;
    Inc(n);
  end;
  pos := 0;
  total := 0;
  parseNode(pos, total, rootValue);
  Writeln(total);
  Writeln(rootValue);
end.