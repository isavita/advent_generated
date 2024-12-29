
program rope;

type
  TPoint = record
    x, y: integer;
  end;

var
  head, tail: TPoint;
  visited: array[-1000..1000, -1000..1000] of boolean;
  line: string;
  dir: char;
  steps, i, count: integer;
  f: text;

function abs(x: integer): integer;
begin
  if x < 0 then
    abs := -x
  else
    abs := x;
end;

begin
  assign(f, 'input.txt');
  reset(f);
  head.x := 0;
  head.y := 0;
  tail.x := 0;
  tail.y := 0;
  visited[0, 0] := true;
  count := 1;

  while not eof(f) do
  begin
    readln(f, line);
    dir := line[1];
    val(copy(line, 3, length(line) - 2), steps);

    for i := 1 to steps do
    begin
      case dir of
        'R': head.x := head.x + 1;
        'L': head.x := head.x - 1;
        'U': head.y := head.y + 1;
        'D': head.y := head.y - 1;
      end;

      if (abs(head.x - tail.x) > 1) or (abs(head.y - tail.y) > 1) then
      begin
        if (head.x <> tail.x) and (head.y <> tail.y) then
        begin
          if head.x > tail.x then
            tail.x := tail.x + 1
          else
            tail.x := tail.x - 1;
          if head.y > tail.y then
            tail.y := tail.y + 1
          else
            tail.y := tail.y - 1;
        end
        else
        begin
          if head.x > tail.x then
            tail.x := tail.x + 1
          else if head.x < tail.x then
            tail.x := tail.x - 1;
          if head.y > tail.y then
            tail.y := tail.y + 1
          else if head.y < tail.y then
            tail.y := tail.y - 1;
        end;
      end;

      if not visited[tail.x, tail.y] then
      begin
        visited[tail.x, tail.y] := true;
        count := count + 1;
      end;
    end;
  end;
  close(f);
  writeln(count);
end.
