
#!/usr/bin/awk -f

# ------------------------------------------------------------
#  Minimum mana to win (hard mode) – Advent of Code 2015 Day 22
#  Reads the boss stats from “input.txt” and prints the answer.
# ------------------------------------------------------------

BEGIN {
    # ---------- read input ----------
    while ((getline < "input.txt") > 0) {
        if ($1 == "Hit")   bossHP = $3
        if ($1 == "Damage:") bossDamage = $2
    }
    close("input.txt")

    # initial player stats (hard mode)
    playerHP   = 50
    playerMana = 500

    minMana = 1e9          # global best

    simulate(playerHP, playerMana, bossHP, 0, 0, 0, 0, 1)   # start with player turn
    print int(minMana)
    exit
}

# ------------------------------------------------------------
#  Recursive simulation
#  pHP  – player hit points
#  pM   – player mana
#  bHP  – boss hit points
#  sh   – shield timer
#  po   – poison timer
#  re   – recharge timer
#  spent – mana spent so far
#  turn  – 1 = player, 0 = boss
# ------------------------------------------------------------
function simulate(pHP, pM, bHP, sh, po, re, spent, turn,
                 dmg, newSpent, newSh, newPo, newRe, newP, newM, newB, i) {

    # prune if already worse than best found
    if (spent >= minMana) return

    # player loses 1 HP at start of his turn (hard mode)
    if (turn) {
        pHP--
        if (pHP <= 0) return
    }

    # ---- apply active effects ----
    if (sh > 0) sh--
    if (po > 0) {
        bHP -= 3
        po--
    }
    if (re > 0) {
        pM += 101
        re--
    }

    # boss dead?
    if (bHP <= 0) {
        if (spent < minMana) minMana = spent
        return
    }

    # player dead?
    if (pHP <= 0) return

    # ------------------- boss turn -------------------
    if (!turn) {
        dmg = bossDamage
        if (sh > 0) dmg -= 7
        if (dmg < 1) dmg = 1
        pHP -= dmg
        simulate(pHP, pM, bHP, sh, po, re, spent, 1)
        return
    }

    # ------------------- player turn -------------------
    # 1. Magic Missile (53)
    if (pM >= 53) {
        newP = pHP
        newM = pM - 53
        newB = bHP - 4
        simulate(newP, newM, newB, sh, po, re, spent + 53, 0)
    }
    # 2. Drain (73)
    if (pM >= 73) {
        newP = pHP + 2
        newM = pM - 73
        newB = bHP - 2
        simulate(newP, newM, newB, sh, po, re, spent + 73, 0)
    }
    # 3. Shield (113) – only if not active
    if (pM >= 113 && sh == 0) {
        newP = pHP
        newM = pM - 113
        simulate(newP, newM, bHP, 6, po, re, spent + 113, 0)
    }
    # 4. Poison (173) – only if not active
    if (pM >= 173 && po == 0) {
        newP = pHP
        newM = pM - 173
        simulate(newP, newM, bHP, sh, 6, re, spent + 173, 0)
    }
    # 5. Recharge (229) – only if not active
    if (pM >= 229 && re == 0) {
        newP = pHP
        newM = pM - 229
        simulate(newP, newM, bHP, sh, po, 5, spent + 229, 0)
    }
}
