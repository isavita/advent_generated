
/* REXX */
main:
  call stream 'input.txt', 'c', 'open read'
  parse value linein('input.txt') with 'Hit Points: ' bossHP
  parse value linein('input.txt') with 'Damage: ' bossDamage
  call stream 'input.txt', 'c', 'close'

  min_mana_spent = 999999

  call simulate 50, 500, bossHP, bossDamage, 0, 0, 0, 0, 1

  say min_mana_spent
exit

simulate:
  procedure expose min_mana_spent
  parse arg pHP, pMana, bHP, bDam, sTimer, pTimer, rTimer, mSpent, is_player_turn

  if mSpent >= min_mana_spent then return
  if bHP <= 0 then do
    min_mana_spent = min(min_mana_spent, mSpent)
    return
  end
  if pHP <= 0 then return

  armor = 0
  if sTimer > 0 then do
    sTimer = sTimer - 1
    armor = 7
  end
  if pTimer > 0 then do
    bHP = bHP - 3
    pTimer = pTimer - 1
  end
  if rTimer > 0 then do
    pMana = pMana + 101
    rTimer = rTimer - 1
  end

  if \is_player_turn then do
    damage = max(1, bDam - armor)
    pHP = pHP - damage
    call simulate pHP, pMana, bHP, bDam, sTimer, pTimer, rTimer, mSpent, 1
    return
  end

  if pMana >= 53 then
    call simulate pHP, pMana - 53, bHP - 4, bDam, sTimer, pTimer, rTimer, mSpent + 53, 0
  if pMana >= 73 then
    call simulate pHP + 2, pMana - 73, bHP - 2, bDam, sTimer, pTimer, rTimer, mSpent + 73, 0
  if pMana >= 113 & sTimer = 0 then
    call simulate pHP, pMana - 113, bHP, bDam, 6, pTimer, rTimer, mSpent + 113, 0
  if pMana >= 173 & pTimer = 0 then
    call simulate pHP, pMana - 173, bHP, bDam, sTimer, 6, rTimer, mSpent + 173, 0
  if pMana >= 229 & rTimer = 0 then
    call simulate pHP, pMana - 229, bHP, bDam, sTimer, pTimer, 5, mSpent + 229, 0
return
