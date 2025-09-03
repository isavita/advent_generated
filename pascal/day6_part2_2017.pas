program MemoryReallocation;

const
  MAX_BANKS = 20;
  MAX_STATES = 10000;

type
  TState = record
    Banks: array[0..MAX_BANKS-1] of Integer;
    Size: Integer;
  end;

  THistory = record
    States: array[0..MAX_STATES-1] of TState;
    Count: Integer;
  end;

function StateEqual(const s1, s2: TState): Boolean;
var
  i: Integer;
begin
  if s1.Size <> s2.Size then
  begin
    StateEqual := False;
    Exit;
  end;
  for i := 0 to s1.Size - 1 do
  begin
    if s1.Banks[i] <> s2.Banks[i] then
    begin
      StateEqual := False;
      Exit;
    end;
  end;
  StateEqual := True;
end;

function FindState(const hist: THistory; const st: TState): Integer;
var
  i: Integer;
begin
  if hist.Count <= 0 then begin
    FindState := -1;
    Exit;
  end;
  for i := 0 to hist.Count - 1 do
  begin
    if StateEqual(hist.States[i], st) then
    begin
      FindState := i;
      Exit;
    end;
  end;
  FindState := -1;
end;

procedure AddState(var hist: THistory; const st: TState);
begin
  if hist.Count < MAX_STATES then
  begin
    hist.States[hist.Count] := st;
    Inc(hist.Count);
  end;
end;

var
  f: Text;
  x, i, size, blocks, max_index, loop_start, cycles: Integer;
  banks: array[0..MAX_BANKS-1] of Integer;
  current_state: TState;
  history: THistory;

begin
  assign(f, 'input.txt');
  {$I-}
  Reset(f);
  {$I+}
  if IOResult <> 0 then
  begin
    WriteLn('Error opening input.txt');
    Halt(1);
  end;

  size := 0;
  while (not Eof(f)) and (size < MAX_BANKS) do
  begin
    Read(f, x);
    banks[size] := x;
    Inc(size);
  end;

  Close(f);

  history.Count := 0;

  current_state.Size := size;
  if size > 0 then Move(banks, current_state.Banks, size * SizeOf(Integer));

  cycles := 0;

  while True do
  begin
    loop_start := FindState(history, current_state);
    if loop_start <> -1 then
    begin
      WriteLn('The size of the loop is ', cycles - loop_start);
      Halt(0);
    end;

    AddState(history, current_state);

    max_index := 0;
    for i := 1 to size - 1 do
    begin
      if current_state.Banks[i] > current_state.Banks[max_index] then
        max_index := i;
    end;

    blocks := current_state.Banks[max_index];
    current_state.Banks[max_index] := 0;
    for i := 1 to blocks do
      current_state.Banks[(max_index + i) mod size] := current_state.Banks[(max_index + i) mod size] + 1;

    Inc(cycles);
  end;
end.