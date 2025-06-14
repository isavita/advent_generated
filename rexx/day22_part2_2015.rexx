
/* Rexx */
main:
  minMana = 999999
  CALL LINEIN 'input.txt'
  PARSE VAR RESULT 'Hit Points:' bossHP
  CALL LINEIN 'input.txt'
  PARSE VAR RESULT 'Damage:' bossDamage
  CALL simulate 50, 500, bossHP, bossDamage, 0, 0, 0, 0, 1
  SAY minMana
EXIT

simulate:
  PROCEDURE EXPOSE minMana
  ARG pHP, pMana, bHP, bDam, sT, pT, rT, spent, isPlayerTurn

  IF spent >= minMana THEN RETURN

  IF isPlayerTurn THEN DO
    pHP = pHP - 1
    IF pHP <= 0 THEN RETURN
  END

  playerArmor = 0
  IF sT > 0 THEN DO; playerArmor = 7; sT = sT - 1; END
  IF pT > 0 THEN DO; bHP = bHP - 3; pT = pT - 1; END
  IF rT > 0 THEN DO; pMana = pMana + 101; rT = rT - 1; END

  IF bHP <= 0 THEN DO
    minMana = MIN(minMana, spent)
    RETURN
  END

  IF \isPlayerTurn THEN DO
    damage = MAX(1, bDam - playerArmor)
    pHP = pHP - damage
    CALL simulate pHP, pMana, bHP, bDam, sT, pT, rT, spent, 1
    RETURN
  END

  IF pMana >= 53  THEN CALL simulate pHP, pMana-53, bHP-4, bDam, sT, pT, rT, spent+53, 0
  IF pMana >= 73  THEN CALL simulate pHP+2, pMana-73, bHP-2, bDam, sT, pT, rT, spent+73, 0
  IF pMana >= 113 & sT=0 THEN CALL simulate pHP, pMana-113, bHP, bDam, 6, pT, rT, spent+113, 0
  IF pMana >= 173 & pT=0 THEN CALL simulate pHP, pMana-173, bHP, bDam, sT, 6, rT, spent+173, 0
  IF pMana >= 229 & rT=0 THEN CALL simulate pHP, pMana-229, bHP, bDam, sT, pT, 5, spent+229, 0
RETURN
