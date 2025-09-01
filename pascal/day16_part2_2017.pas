
program Dance;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

const
  PROGRAMS_LEN = 16;
  ITERATIONS   = 1000000000;

type
  TProgram = array[0..PROGRAMS_LEN-1] of Char;

  TMoveKind = (mkSpin, mkExchange, mkPartner);

  TMove = record
    Kind : TMoveKind;
    A,B  : Integer;   // used for spin (A) and exchange (A,B)
    CA,CB: Char;      // used for partner
  end;

var
  Moves      : array of TMove;
  Line       : string;
  Initial    : TProgram;
  Programs   : TProgram;
  Temp       : TProgram;
  i, j, CycleLen, Rest : Integer;
  MoveIdx    : Integer;
  Found      : Boolean;

//--------------------------------------------------------------------
procedure Spin(var P: TProgram; X: Integer);
var
  k, src: Integer;
begin
  X := X mod PROGRAMS_LEN;
  for k := 0 to PROGRAMS_LEN-1 do
  begin
    src := (k + PROGRAMS_LEN - X) mod PROGRAMS_LEN;
    Temp[k] := P[src];
  end;
  P := Temp;
end;

procedure Exchange(var P: TProgram; A, B: Integer);
var
  C: Char;
begin
  C := P[A];
  P[A] := P[B];
  P[B] := C;
end;

procedure Partner(var P: TProgram; CA, CB: Char);
var
  AIdx, BIdx, k: Integer;
begin
  AIdx := -1;
  BIdx := -1;
  for k := 0 to PROGRAMS_LEN-1 do
  begin
    if P[k] = CA then AIdx := k;
    if P[k] = CB then BIdx := k;
  end;
  if (AIdx <> -1) and (BIdx <> -1) then
    Exchange(P, AIdx, BIdx);
end;

procedure ApplyMove(var P: TProgram; const M: TMove);
begin
  case M.Kind of
    mkSpin    : Spin(P, M.A);
    mkExchange: Exchange(P, M.A, M.B);
    mkPartner : Partner(P, M.CA, M.CB);
  end;
end;

function ProgramsEqual(const A, B: TProgram): Boolean;
var
  k: Integer;
begin
  for k := 0 to PROGRAMS_LEN-1 do
    if A[k] <> B[k] then
      Exit(False);
  Result := True;
end;

function ProgramToString(const P: TProgram): string;
begin
  SetLength(Result, PROGRAMS_LEN);
  Move(P[0], Result[1], PROGRAMS_LEN);
end;

//--------------------------------------------------------------------
procedure ParseInput;
var
  Token, RestStr: string;
  PosSlash: Integer;
  M: TMove;
begin
  RestStr := Line;
  while RestStr <> '' do
  begin
    PosSlash := Pos(',', RestStr);
    if PosSlash = 0 then
    begin
      Token := RestStr;
      RestStr := '';
    end
    else
    begin
      Token := Copy(RestStr, 1, PosSlash-1);
      Delete(RestStr, 1, PosSlash);
    end;

    case Token[1] of
      's': begin
             M.Kind := mkSpin;
             M.A := StrToInt(Copy(Token, 2, Length(Token)-1));
           end;
      'x': begin
             M.Kind := mkExchange;
             PosSlash := Pos('/', Token);
             M.A := StrToInt(Copy(Token, 2, PosSlash-2));
             M.B := StrToInt(Copy(Token, PosSlash+1, Length(Token)-PosSlash));
           end;
      'p': begin
             M.Kind := mkPartner;
             PosSlash := Pos('/', Token);
             M.CA := Token[2];
             M.CB := Token[PosSlash+1];
           end;
    end;
    SetLength(Moves, Length(Moves)+1);
    Moves[High(Moves)] := M;
  end;
end;

//--------------------------------------------------------------------
begin
  // read whole line from input.txt
  with TStringList.Create do
    try
      LoadFromFile('input.txt');
      if Count = 0 then Halt(1);
      Line := Trim(Strings[0]);
    finally
      Free;
    end;

  // initial program order a..p
  for i := 0 to PROGRAMS_LEN-1 do
    Initial[i] := Chr(Ord('a') + i);
  Programs := Initial;

  // parse moves once
  ParseInput;

  // find cycle length
  CycleLen := 0;
  for i := 0 to ITERATIONS-1 do
  begin
    for MoveIdx := 0 to High(Moves) do
      ApplyMove(Programs, Moves[MoveIdx]);

    if ProgramsEqual(Programs, Initial) then
    begin
      CycleLen := i + 1;
      Break;
    end;
  end;

  // if a cycle was found, reduce remaining iterations
  if CycleLen > 0 then
    Rest := ITERATIONS mod CycleLen
  else
    Rest := ITERATIONS; // should not happen

  // reset to initial state
  Programs := Initial;

  // perform the remaining iterations
  for i := 0 to Rest-1 do
    for MoveIdx := 0 to High(Moves) do
      ApplyMove(Programs, Moves[MoveIdx]);

  // output result
  WriteLn(ProgramToString(Programs));
end.
