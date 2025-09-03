program FindNonOverlappingClaim;

uses
  SysUtils;

type
  Claim = record
    ID, X, Y, W, H: Integer;
  end;

var
  fabric: array[0..999, 0..999] of Integer;
  claims: array of Claim;

procedure ParseLine(line: string; var ok: Boolean; var id, x, y, w, h: Integer);
var
  s: string;
  i, cnt, v, code: Integer;
  tmp: string;
  nums: array[1..5] of Integer;
begin
  s := line;
  for i := 1 to Length(s) do
    case s[i] of
      '#', '@', ',', ':', 'x': s[i] := ' ';
    end;

  cnt := 0;
  i := 1;
  while i <= Length(s) do begin
    while (i <= Length(s)) and (s[i] = ' ') do inc(i);
    if i > Length(s) then Break;
    tmp := '';
    while (i <= Length(s)) and (s[i] <> ' ') do begin
      tmp := tmp + s[i];
      inc(i);
    end;
    val(tmp, v, code);
    if code = 0 then begin
      inc(cnt);
      nums[cnt] := v;
    end;
  end;
  if cnt = 5 then begin
    id := nums[1];
    x  := nums[2];
    y  := nums[3];
    w  := nums[4];
    h  := nums[5];
    ok := True;
  end else begin
    ok := False;
  end;
end;

var
  f: Text;
  line: string;
  count, i, j, y, x, w, h: Integer;
  okLine: Boolean;
  id, cx, cy, cw, ch: Integer;
  found: Boolean;

begin
  for i := 0 to 999 do
    for j := 0 to 999 do
      fabric[i, j] := 0;

  assign(f, 'input.txt');
  reset(f);
  SetLength(claims, 0);
  count := 0;

  while not Eof(f) do begin
    ReadLn(f, line);
    ParseLine(line, okLine, id, cx, cy, cw, ch);
    if okLine then begin
      inc(count);
      SetLength(claims, count);
      claims[count - 1].ID := id;
      claims[count - 1].X := cx;
      claims[count - 1].Y := cy;
      claims[count - 1].W := cw;
      claims[count - 1].H := ch;
    end;
  end;
  Close(f);

  for i := 0 to count - 1 do begin
    for y := claims[i].Y to claims[i].Y + claims[i].H - 1 do
      for x := claims[i].X to claims[i].X + claims[i].W - 1 do
        inc(fabric[y, x]);
  end;

  for i := 0 to count - 1 do begin
    found := False;
    for y := claims[i].Y to claims[i].Y + claims[i].H - 1 do begin
      if not found then
        for x := claims[i].X to claims[i].X + claims[i].W - 1 do begin
          if fabric[y, x] > 1 then begin
            found := True;
            Break;
          end;
        end;
    end;
    if not found then begin
      Writeln(claims[i].ID);
      Break;
    end;
  end;
end.