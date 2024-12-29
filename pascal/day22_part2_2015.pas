
program AdventOfCode;

{$O+,Q+,R+,S+}

type
  GameState = record
    playerHP, playerMana, bossHP, bossDamage: Integer;
    shieldTimer, poisonTimer, rechargeTimer: Integer;
    manaSpent: Integer;
  end;

var
  minMana: Integer;
  bossHP, bossDamage: Integer;
  input: Text;

procedure Simulate(state: GameState; playerTurn: Boolean);
var
  newState: GameState;
  damage: Integer;
begin
  if state.manaSpent >= minMana then
    Exit;
  if state.bossHP <= 0 then
  begin
    minMana := state.manaSpent;
    Exit;
  end;
  if state.playerHP <= 0 then
    Exit;

  if playerTurn then
  begin
    state.playerHP := state.playerHP - 1;
    if state.playerHP <= 0 then
      Exit;
  end;

  if state.shieldTimer > 0 then
    state.shieldTimer := state.shieldTimer - 1;
  if state.poisonTimer > 0 then
  begin
    state.bossHP := state.bossHP - 3;
    state.poisonTimer := state.poisonTimer - 1;
  end;
  if state.rechargeTimer > 0 then
  begin
    state.playerMana := state.playerMana + 101;
    state.rechargeTimer := state.rechargeTimer - 1;
  end;

  if not playerTurn then
  begin
    damage := state.bossDamage;
    if state.shieldTimer > 0 then
      damage := damage - 7;
    if damage < 1 then
      damage := 1;
    state.playerHP := state.playerHP - damage;
    Simulate(state, True);
    Exit;
  end;

  if state.playerMana >= 53 then
  begin
    newState := state;
    newState.playerMana := newState.playerMana - 53;
    newState.manaSpent := newState.manaSpent + 53;
    newState.bossHP := newState.bossHP - 4;
    Simulate(newState, False);
  end;
  if state.playerMana >= 73 then
  begin
    newState := state;
    newState.playerMana := newState.playerMana - 73;
    newState.manaSpent := newState.manaSpent + 73;
    newState.bossHP := newState.bossHP - 2;
    newState.playerHP := newState.playerHP + 2;
    Simulate(newState, False);
  end;
  if (state.playerMana >= 113) and (state.shieldTimer = 0) then
  begin
    newState := state;
    newState.playerMana := newState.playerMana - 113;
    newState.manaSpent := newState.manaSpent + 113;
    newState.shieldTimer := 6;
    Simulate(newState, False);
  end;
  if (state.playerMana >= 173) and (state.poisonTimer = 0) then
  begin
    newState := state;
    newState.playerMana := newState.playerMana - 173;
    newState.manaSpent := newState.manaSpent + 173;
    newState.poisonTimer := 6;
    Simulate(newState, False);
  end;
  if (state.playerMana >= 229) and (state.rechargeTimer = 0) then
  begin
    newState := state;
    newState.playerMana := newState.playerMana - 229;
    newState.manaSpent := newState.manaSpent + 229;
    newState.rechargeTimer := 5;
    Simulate(newState, False);
  end;
end;

function MinManaToWin(initialState: GameState): Integer;
begin
  minMana := MaxInt;
  initialState.playerHP := 50;
  initialState.playerMana := 500;
  Simulate(initialState, True);
  MinManaToWin := minMana;
end;

var
  line: string;
  initialState: GameState;
begin
  Assign(input, 'input.txt');
  Reset(input);
  ReadLn(input, line);
  Val(Copy(line, Pos(': ', line) + 2, Length(line)), bossHP);
  ReadLn(input, line);
  Val(Copy(line, Pos(': ', line) + 2, Length(line)), bossDamage);
  Close(input);

  initialState.bossHP := bossHP;
  initialState.bossDamage := bossDamage;
  WriteLn(MinManaToWin(initialState));
end.
