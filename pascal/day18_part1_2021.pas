
program SnailfishMagnitude;
{$mode objfpc}{$H+}

uses
  SysUtils;

type
  TNum = record
    v, d: Integer;   // v = regular value, d = nesting depth
  end;
  TArr = array of TNum;

{ parse one line like "[[1,2],3]" into a flat list of (value,depth) }
function ParseLine(const s: string): TArr;
var
  i, depth, num, L: Integer;
  c: Char;
begin
  depth := 0;
  L := Length(s);
  SetLength(Result, 0);
  i := 1;
  while i <= L do
  begin
    c := s[i];
    case c of
      '[': Inc(depth);
      ']': Dec(depth);
      '0'..'9':
        begin
          num := 0;
          while (i <= L) and (s[i] in ['0'..'9']) do
          begin
            num := num * 10 + Ord(s[i]) - Ord('0');
            Inc(i);
          end;
          // append (num,depth)
          SetLength(Result, Length(Result)+1);
          Result[High(Result)].v := num;
          Result[High(Result)].d := depth;
          Continue; // already advanced i
        end;
    end;
    Inc(i);
  end;
end;

{ perform a full reduce: repeatedly explode then split until no action applies }
procedure Reduce(var A: TArr);
var
  i, L: Integer;
  exploded, splitted: Boolean;
  leftv, rightv, thisd: Integer;
begin
  repeat
    exploded := False;
    L := Length(A);
    // 1) explode?
    for i := 0 to L-2 do
      if (A[i].d >= 5) and (A[i].d = A[i+1].d) then
      begin
        // add to left neighbor
        if i > 0 then
          A[i-1].v := A[i-1].v + A[i].v;
        // add to right neighbor
        if i+2 < L then
          A[i+2].v := A[i+2].v + A[i+1].v;
        // replace pair by single 0 at depth-1
        A[i].v := 0;
        A[i].d := A[i].d - 1;
        // remove A[i+1]
        Move(A[i+2], A[i+1], (L-(i+2))*SizeOf(TNum));
        SetLength(A, L-1);
        exploded := True;
        Break;
      end;
    if exploded then
      Continue; // after an explode, restart

    // 2) split?
    splitted := False;
    L := Length(A);
    for i := 0 to L-1 do
      if A[i].v >= 10 then
      begin
        leftv := A[i].v div 2;
        rightv := A[i].v - leftv;
        thisd := A[i].d + 1;
        // expand array by 1
        SetLength(A, L+1);
        // shift tail right
        Move(A[i+1], A[i+2], (L-(i+1))*SizeOf(TNum));
        // insert the two halves
        A[i].v := leftv;  A[i].d := thisd;
        A[i+1].v := rightv; A[i+1].d := thisd;
        splitted := True;
        Break;
      end;
    if not splitted then
      Break; // fully reduced
  until False;
end;

{ add two snailfish numbers (flat-list form), then reduce }
function AddSnail(const A, B: TArr): TArr;
var
  i, na, nb: Integer;
begin
  na := Length(A);
  nb := Length(B);
  SetLength(Result, na + nb);
  // shift depths up by 1
  for i := 0 to na-1 do
  begin
    Result[i].v := A[i].v;
    Result[i].d := A[i].d + 1;
  end;
  for i := 0 to nb-1 do
  begin
    Result[na+i].v := B[i].v;
    Result[na+i].d := B[i].d + 1;
  end;
  Reduce(Result);
end;

{ compute magnitude by repeatedly collapsing the deepest adjacent pair }
function Magnitude(var A: TArr): Integer;
var
  i, L, maxd: Integer;
begin
  repeat
    L := Length(A);
    if L = 1 then
    begin
      Result := A[0].v;
      Exit;
    end;
    // find current max depth
    maxd := 0;
    for i := 0 to L-1 do
      if A[i].d > maxd then
        maxd := A[i].d;
    // find first adjacent pair at that depth
    for i := 0 to L-2 do
      if (A[i].d = maxd) and (A[i+1].d = maxd) then
      begin
        A[i].v := 3*A[i].v + 2*A[i+1].v;
        A[i].d := maxd - 1;
        // remove A[i+1]
        Move(A[i+2], A[i+1], (L-(i+2))*SizeOf(TNum));
        SetLength(A, L-1);
        Break;
      end;
  until False;
end;

var
  F: TextFile;
  line: string;
  sum, nxt: TArr;
  first: Boolean;
  mag: Integer;
begin
  AssignFile(F, 'input.txt');
  Reset(F);
  first := True;
  while not Eof(F) do
  begin
    ReadLn(F, line);
    if line = '' then Continue;
    if first then
    begin
      sum := ParseLine(line);
      first := False;
    end
    else
    begin
      nxt := ParseLine(line);
      sum := AddSnail(sum, nxt);
    end;
  end;
  CloseFile(F);

  mag := Magnitude(sum);
  WriteLn(mag);
end.
