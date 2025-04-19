program CountTrees;
uses Classes, SysUtils;
const R: array[0..4] of Integer = (1,3,5,7,1);
      D: array[0..4] of Integer = (1,1,1,1,2);
var L: TStringList;
    i, h, w, row, col: Integer;
    cnt: Int64;
    prod: Int64;
begin
  L := TStringList.Create;
  L.LoadFromFile('input.txt');
  h := L.Count;
  w := Length(L[0]);
  prod := 1;
  for i := 0 to 4 do
  begin
    cnt := 0; col := 0; row := 0;
    while row < h do
    begin
      if L[row][(col mod w) + 1] = '#' then Inc(cnt);
      Inc(col, R[i]);
      Inc(row, D[i]);
    end;
    prod := prod * cnt;
  end;
  WriteLn(prod);
end.