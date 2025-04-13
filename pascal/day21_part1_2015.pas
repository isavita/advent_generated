
program DndShop;

const
  Weapons: array[1..5, 1..4] of integer = (
    (8, 4, 0, 0),
    (10, 5, 0, 0),
    (25, 6, 0, 0),
    (40, 7, 0, 0),
    (74, 8, 0, 0)
  );
  Armor: array[1..6, 1..4] of integer = (
    (0, 0, 0, 0),
    (13, 0, 1, 0),
    (31, 0, 2, 0),
    (53, 0, 3, 0),
    (75, 0, 4, 0),
    (102, 0, 5, 0)
  );
  Rings: array[1..7, 1..4] of integer = (
    (0, 0, 0, 0),
    (25, 1, 0, 0),
    (50, 2, 0, 0),
    (100, 3, 0, 0),
    (20, 0, 1, 0),
    (40, 0, 2, 0),
    (80, 0, 3, 0)
  );

  BossHP = 103;
  BossDamage = 9;
  BossArmor = 2;

  PlayerHP = 100;

var
  WeaponIndex, ArmorIndex, Ring1Index, Ring2Index: integer;
  Cost, PlayerDamage, PlayerArmor, PlayerTurns, BossTurns: integer;
  MinGold: integer;

function TurnsToDefeat(attackerDamage, defenderHP, defenderArmor: integer): integer;
var
  DamageDealt: integer;
begin
  DamageDealt := attackerDamage - defenderArmor;
  if DamageDealt < 1 then
    DamageDealt := 1;
  TurnsToDefeat := (defenderHP + DamageDealt - 1) div DamageDealt;
end;

begin
  MinGold := MaxInt;

  for WeaponIndex := 1 to 5 do
  begin
    for ArmorIndex := 1 to 6 do
    begin
      for Ring1Index := 1 to 7 do
      begin
        for Ring2Index := Ring1Index + 1 to 7 do
        begin
          Cost := Weapons[WeaponIndex, 1] + Armor[ArmorIndex, 1] + Rings[Ring1Index, 1] + Rings[Ring2Index, 1];
          PlayerDamage := Weapons[WeaponIndex, 2] + Armor[ArmorIndex, 2] + Rings[Ring1Index, 2] + Rings[Ring2Index, 2];
          PlayerArmor := Weapons[WeaponIndex, 3] + Armor[ArmorIndex, 3] + Rings[Ring1Index, 3] + Rings[Ring2Index, 3];

          PlayerTurns := TurnsToDefeat(PlayerDamage, BossHP, BossArmor);
          BossTurns := TurnsToDefeat(BossDamage, PlayerHP, PlayerArmor);

          if PlayerTurns <= BossTurns then
          begin
            if Cost < MinGold then
              MinGold := Cost;
          end;
        end;
      end;
    end;
  end;

  Writeln(MinGold);
end.
