
program combat;

type
  GameState = record
    playerHP, playerMana, bossHP, bossDamage: integer;
    shieldTimer, poisonTimer, rechargeTimer: integer;
    manaSpent: integer;
  end;

var
  minMana: integer;
  bossHP, bossDamage: integer;
  input: text;

procedure simulate(state: GameState; playerTurn: boolean);
var
  newState: GameState;
  damage: integer;
begin
  if state.manaSpent >= minMana then
    exit;
  if state.bossHP <= 0 then
  begin
    minMana := state.manaSpent;
    exit;
  end;
  if state.playerHP <= 0 then
    exit;

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
    simulate(state, true);
    exit;
  end;

  if state.playerMana >= 53 then
  begin
    newState := state;
    newState.playerMana := newState.playerMana - 53;
    newState.manaSpent := newState.manaSpent + 53;
    newState.bossHP := newState.bossHP - 4;
    simulate(newState, false);
  end;
  if state.playerMana >= 73 then
  begin
    newState := state;
    newState.playerMana := newState.playerMana - 73;
    newState.manaSpent := newState.manaSpent + 73;
    newState.bossHP := newState.bossHP - 2;
    newState.playerHP := newState.playerHP + 2;
    simulate(newState, false);
  end;
  if (state.playerMana >= 113) and (state.shieldTimer = 0) then
  begin
    newState := state;
    newState.playerMana := newState.playerMana - 113;
    newState.manaSpent := newState.manaSpent + 113;
    newState.shieldTimer := 6;
    simulate(newState, false);
  end;
  if (state.playerMana >= 173) and (state.poisonTimer = 0) then
  begin
    newState := state;
    newState.playerMana := newState.playerMana - 173;
    newState.manaSpent := newState.manaSpent + 173;
    newState.poisonTimer := 6;
    simulate(newState, false);
  end;
  if (state.playerMana >= 229) and (state.rechargeTimer = 0) then
  begin
    newState := state;
    newState.playerMana := newState.playerMana - 229;
    newState.manaSpent := newState.manaSpent + 229;
    newState.rechargeTimer := 5;
    simulate(newState, false);
  end;
end;

function minManaToWin(initialState: GameState): integer;
begin
  minMana := maxint;
  simulate(initialState, true);
  minManaToWin := minMana;
end;

var
  initialState: GameState;
  line: string;
begin
  assign(input, 'input.txt');
  reset(input);
  readln(input, line);
  val(copy(line, pos(': ', line) + 2, length(line)), bossHP);
  readln(input, line);
  val(copy(line, pos(': ', line) + 2, length(line)), bossDamage);
  close(input);

  initialState.bossHP := bossHP;
  initialState.bossDamage := bossDamage;
  initialState.playerHP := 50;
  initialState.playerMana := 500;
  initialState.shieldTimer := 0;
  initialState.poisonTimer := 0;
  initialState.rechargeTimer := 0;
  initialState.manaSpent := 0;

  writeln(minManaToWin(initialState));
end.
