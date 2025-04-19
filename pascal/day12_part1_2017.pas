program CountGroup0;
uses sysutils;
var
  pipes: array of array of integer;
  visited: array of boolean;
  f: Text;
  s, numstr: string;
  nums: array of integer;
  i, j, n: integer;

procedure DFS(u: integer);
var
  v: integer;
begin
  visited[u] := true;
  for v in pipes[u] do
    if not visited[v] then
      DFS(v);
end;

begin
  Assign(f, 'input.txt'); Reset(f);
  while not eof(f) do
  begin
    ReadLn(f, s);
    nums := [];
    i := 1;
    while i <= Length(s) do
      if s[i] in ['0'..'9'] then
      begin
        numstr := '';
        while (i <= Length(s)) and (s[i] in ['0'..'9']) do
        begin
          numstr := numstr + s[i];
          Inc(i);
        end;
        SetLength(nums, Length(nums) + 1);
        nums[High(nums)] := StrToInt(numstr);
      end
      else
        Inc(i);
    if Length(nums) > 0 then
    begin
      if nums[0] > High(pipes) then
        SetLength(pipes, nums[0] + 1);
      SetLength(pipes[nums[0]], Length(nums) - 1);
      for j := 1 to High(nums) do
        pipes[nums[0]][j - 1] := nums[j];
    end;
  end;
  Close(f);

  SetLength(visited, Length(pipes));
  DFS(0);

  n := 0;
  for i := 0 to High(visited) do
    if visited[i] then
      Inc(n);
  WriteLn(n);
end.