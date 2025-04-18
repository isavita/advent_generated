program PermutationPromenade;

{$mode objfpc}{$H+}

uses
  SysUtils;

var
  moves: string;
  arr, tempArr: array[0..15] of char;
  i, lenM, pos, A, B, num: Integer;
  cmd: Char;
  numStr: string;
  c1, c2: Char;

begin
  Assign(input, 'input.txt'); Reset(input);
  ReadLn(moves);
  // initialize programs a..p
  for i := 0 to 15 do
    arr[i] := Chr(Ord('a') + i);

  lenM := Length(moves);
  pos := 1;
  while pos <= lenM do
  begin
    cmd := moves[pos];
    Inc(pos);
    case cmd of
      's': begin
        // parse spin size
        numStr := '';
        while (pos <= lenM) and (moves[pos] in ['0'..'9']) do
        begin
          numStr := numStr + moves[pos];
          Inc(pos);
        end;
        num := StrToInt(numStr);
        // perform spin
        for i := 0 to 15 do
          tempArr[(i + num) mod 16] := arr[i];
        for i := 0 to 15 do
          arr[i] := tempArr[i];
      end;
      'x': begin
        // parse positions A/B
        numStr := '';
        while (pos <= lenM) and (moves[pos] in ['0'..'9']) do
        begin
          numStr := numStr + moves[pos];
          Inc(pos);
        end;
        A := StrToInt(numStr);
        Inc(pos); // skip '/'
        numStr := '';
        while (pos <= lenM) and (moves[pos] in ['0'..'9']) do
        begin
          numStr := numStr + moves[pos];
          Inc(pos);
        end;
        B := StrToInt(numStr);
        // swap by index
        c1 := arr[A];
        arr[A] := arr[B];
        arr[B] := c1;
      end;
      'p': begin
        // parse programs A/B
        c1 := moves[pos]; Inc(pos);
        Inc(pos); // skip '/'
        c2 := moves[pos]; Inc(pos);
        // find and swap by name
        A := 0;
        while arr[A] <> c1 do Inc(A);
        B := 0;
        while arr[B] <> c2 do Inc(B);
        arr[A] := c2;
        arr[B] := c1;
      end;
    end;
    // skip comma
    if (pos <= lenM) and (moves[pos] = ',') then
      Inc(pos);
  end;

  // output final order
  for i := 0 to 15 do
    Write(arr[i]);
  WriteLn;
end.