
open System
open System.IO

type GameState =
    { playerHP      : int
      playerMana    : int
      bossHP        : int
      bossDamage    : int
      shieldTimer   : int
      poisonTimer   : int
      rechargeTimer : int
      manaSpent     : int }

[<EntryPoint>]
let main _ =
    // read boss stats
    let lines = File.ReadAllLines("input.txt")
    let bossHP     = Int32.Parse(lines[0].Split(": ")[1])
    let bossDamage = Int32.Parse(lines[1].Split(": ")[1])

    // initial state
    let initState = 
        { playerHP = 50
          playerMana = 500
          bossHP = bossHP
          bossDamage = bossDamage
          shieldTimer = 0
          poisonTimer = 0
          rechargeTimer = 0
          manaSpent = 0 }

    // mutable best result
    let mutable best = Int32.MaxValue

    // apply timers and effects
    let applyEffects (s:GameState) =
        let shield = if s.shieldTimer > 0 then s.shieldTimer - 1 else 0
        let poison = if s.poisonTimer > 0 then s.poisonTimer - 1 else 0
        let recharge = if s.rechargeTimer > 0 then s.rechargeTimer - 1 else 0
        let bossHP  = s.bossHP - (if s.poisonTimer > 0 then 3 else 0)
        let playerMana = s.playerMana + (if s.rechargeTimer > 0 then 101 else 0)
        { s with
            shieldTimer = shield
            poisonTimer = poison
            rechargeTimer = recharge
            bossHP = bossHP
            playerMana = playerMana }

    // recursive search
    let rec dfs (state:GameState) (playerTurn:bool) =
        // prune if already worse
        if state.manaSpent >= best then ()
        elif state.bossHP <= 0 then
            best <- min best state.manaSpent
        elif state.playerHP <= 0 then
            ()
        else
            let state = applyEffects state
            if state.bossHP <= 0 then
                best <- min best state.manaSpent
            elif state.playerHP <= 0 then
                ()
            elif not playerTurn then
                // boss turn
                let dmg =
                    let d = state.bossDamage - (if state.shieldTimer > 0 then 7 else 0)
                    if d < 1 then 1 else d
                let state = { state with playerHP = state.playerHP - dmg }
                dfs state true
            else
                // player turn: try each spell
                // Magic Missile
                if state.playerMana >= 53 then
                    let s = { state with
                                playerMana = state.playerMana - 53
                                manaSpent = state.manaSpent + 53
                                bossHP = state.bossHP - 4 }
                    dfs s false
                // Drain
                if state.playerMana >= 73 then
                    let s = { state with
                                playerMana = state.playerMana - 73
                                manaSpent = state.manaSpent + 73
                                bossHP = state.bossHP - 2
                                playerHP = state.playerHP + 2 }
                    dfs s false
                // Shield
                if state.playerMana >= 113 && state.shieldTimer = 0 then
                    let s = { state with
                                playerMana = state.playerMana - 113
                                manaSpent = state.manaSpent + 113
                                shieldTimer = 6 }
                    dfs s false
                // Poison
                if state.playerMana >= 173 && state.poisonTimer = 0 then
                    let s = { state with
                                playerMana = state.playerMana - 173
                                manaSpent = state.manaSpent + 173
                                poisonTimer = 6 }
                    dfs s false
                // Recharge
                if state.playerMana >= 229 && state.rechargeTimer = 0 then
                    let s = { state with
                                playerMana = state.playerMana - 229
                                manaSpent = state.manaSpent + 229
                                rechargeTimer = 5 }
                    dfs s false

    dfs initState true
    printfn "%d" best
    0
