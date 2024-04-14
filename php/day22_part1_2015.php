<?php

class GameState {
    public $playerHP = 50;
    public $playerMana = 500;
    public $bossHP;
    public $bossDamage;
    public $shieldTimer = 0;
    public $poisonTimer = 0;
    public $rechargeTimer = 0;
    public $manaSpent = 0;

    public function __construct($bossHP, $bossDamage) {
        $this->bossHP = $bossHP;
        $this->bossDamage = $bossDamage;
    }
}

function minManaToWin($initialState) {
    $minMana = PHP_INT_MAX;
    $simulate = function($state, $playerTurn) use (&$simulate, &$minMana) {
        if ($state->manaSpent >= $minMana) {
            return;
        }
        if ($state->bossHP <= 0) {
            $minMana = $state->manaSpent;
            return;
        }
        if ($state->playerHP <= 0) {
            return;
        }

        // Apply effects
        if ($state->shieldTimer > 0) {
            $state->shieldTimer--;
        }
        if ($state->poisonTimer > 0) {
            $state->bossHP -= 3;
            $state->poisonTimer--;
        }
        if ($state->rechargeTimer > 0) {
            $state->playerMana += 101;
            $state->rechargeTimer--;
        }

        if (!$playerTurn) {
            $damage = $state->bossDamage;
            if ($state->shieldTimer > 0) {
                $damage -= 7;
            }
            $damage = max($damage, 1);
            $state->playerHP -= $damage;
            $simulate(clone $state, true);
            return;
        }

        $costs = [53, 73, 113, 173, 229];
        $effects = [
            function($s) { $s->bossHP -= 4; },
            function($s) { $s->bossHP -= 2; $s->playerHP += 2; },
            function($s) { if ($s->shieldTimer == 0) $s->shieldTimer = 6; else return false; },
            function($s) { if ($s->poisonTimer == 0) $s->poisonTimer = 6; else return false; },
            function($s) { if ($s->rechargeTimer == 0) $s->rechargeTimer = 5; else return false; }
        ];

        foreach ($costs as $i => $cost) {
            if ($state->playerMana >= $cost) {
                $newState = clone $state;
                $newState->playerMana -= $cost;
                $newState->manaSpent += $cost;
                if ($effects[$i]($newState) !== false) {
                    $simulate($newState, false);
                }
            }
        }
    };

    $simulate($initialState, true);
    return $minMana;
}

$file = fopen("input.txt", "r");
fscanf($file, "Hit Points: %d", $bossHP);
fscanf($file, "Damage: %d", $bossDamage);
fclose($file);

$initialState = new GameState($bossHP, $bossDamage);
echo minManaToWin($initialState);
?>