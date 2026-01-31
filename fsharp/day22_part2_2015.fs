
open System
open System.IO

type GameState = {
    playerHP:int
    playerMana:int
    bossHP:int
    bossDamage:int
    shieldTimer:int
    poisonTimer:int
    rechargeTimer:int
    manaSpent:int
}

let mutable minMana = Int32.MaxValue

let rec simulate (state:GameState) playerTurn =
    if state.manaSpent >= minMana then ()
    elif state.bossHP <= 0 then minMana <- state.manaSpent
    elif state.playerHP <= 0 then ()
    else
        let state =
            if playerTurn then
                { state with playerHP = state.playerHP - 1 }
            else state
        if state.playerHP <= 0 then ()
        else
            let sShield = if state.shieldTimer > 0 then state.shieldTimer - 1 else 0
            let sPoison, sBossHP =
                if state.poisonTimer > 0 then state.poisonTimer - 1, state.bossHP - 3
                else state.poisonTimer, state.bossHP
            let sRecharge, sMana =
                if state.rechargeTimer > 0 then state.rechargeTimer - 1, state.playerMana + 101
                else state.rechargeTimer, state.playerMana
            let baseState = { state with shieldTimer = sShield; poisonTimer = sPoison; rechargeTimer = sRecharge; playerMana = sMana; bossHP = sBossHP }
            if not playerTurn then
                let dmg = max 1 (baseState.bossDamage - (if baseState.shieldTimer > 0 then 7 else 0))
                simulate { baseState with playerHP = baseState.playerHP - dmg } true
            else
                let trySpell cost effect =
                    if baseState.playerMana >= cost then
                        let ns = effect { baseState with playerMana = baseState.playerMana - cost; manaSpent = baseState.manaSpent + cost }
                        simulate ns false
                trySpell 53 (fun s -> { s with bossHP = s.bossHP - 4 })
                trySpell 73 (fun s -> { s with bossHP = s.bossHP - 2; playerHP = s.playerHP + 2 })
                if baseState.shieldTimer = 0 then trySpell 113 (fun s -> { s with shieldTimer = 6 })
                if baseState.poisonTimer = 0 then trySpell 173 (fun s -> { s with poisonTimer = 6 })
                if baseState.rechargeTimer = 0 then trySpell 229 (fun s -> { s with rechargeTimer = 5 })

let minManaToWin bossHP bossDamage =
    let init = { playerHP = 50; playerMana = 500; bossHP = bossHP; bossDamage = bossDamage
                 shieldTimer = 0; poisonTimer = 0; rechargeTimer = 0; manaSpent = 0 }
    simulate init true
    minMana

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    let bossHP = lines.[0].Split([|':'|]) |> Array.last |> Int32.Parse
    let bossDamage = lines.[1].Split([|':'|]) |> Array.last |> Int32.Parse
    printfn "%d" (minManaToWin bossHP bossDamage)
    0
