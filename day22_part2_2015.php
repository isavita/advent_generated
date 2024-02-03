
<?php

class GameState {
    public $playerHP, $playerMana, $bossHP, $bossDamage;
    public $shieldTimer, $poisonTimer, $rechargeTimer;
    public $manaSpent;

    public function __construct($playerHP, $playerMana, $bossHP, $bossDamage, $shieldTimer, $poisonTimer, $rechargeTimer, $manaSpent) {
        $this->playerHP = $playerHP;
        $this->playerMana = $playerMana;
        $this->bossHP = $bossHP;
        $this->bossDamage = $bossDamage;
        $this->shieldTimer = $shieldTimer;
        $this->poisonTimer = $poisonTimer;
        $this->rechargeTimer = $rechargeTimer;
        $this->manaSpent = $manaSpent;
    }
}

function minManaToWin($initialState) {
    $minMana = PHP_INT_MAX;
    $simulate = function ($state, $playerTurn) use (&$minMana, &$simulate) {
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

        if ($playerTurn) {
            $state->playerHP--;
            if ($state->playerHP <= 0) {
                return;
            }
        }

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
            if ($damage < 1) {
                $damage = 1;
            }
            $state->playerHP -= $damage;
            $simulate($state, true);
            return;
        }

        if ($state->playerMana >= 53) {
            $newState = clone $state;
            $newState->playerMana -= 53;
            $newState->manaSpent += 53;
            $newState->bossHP -= 4;
            $simulate($newState, false);
        }
        if ($state->playerMana >= 73) {
            $newState = clone $state;
            $newState->playerMana -= 73;
            $newState->manaSpent += 73;
            $newState->bossHP -= 2;
            $newState->playerHP += 2;
            $simulate($newState, false);
        }
        if ($state->playerMana >= 113 && $state->shieldTimer == 0) {
            $newState = clone $state;
            $newState->playerMana -= 113;
            $newState->manaSpent += 113;
            $newState->shieldTimer = 6;
            $simulate($newState, false);
        }
        if ($state->playerMana >= 173 && $state->poisonTimer == 0) {
            $newState = clone $state;
            $newState->playerMana -= 173;
            $newState->manaSpent += 173;
            $newState->poisonTimer = 6;
            $simulate($newState, false);
        }
        if ($state->playerMana >= 229 && $state->rechargeTimer == 0) {
            $newState = clone $state;
            $newState->playerMana -= 229;
            $newState->manaSpent += 229;
            $newState->rechargeTimer = 5;
            $simulate($newState, false);
        }
    };

    $initialState->playerHP = 50;
    $initialState->playerMana = 500;
    $simulate($initialState, true);
    return $minMana;
}

$lines = file("input.txt", FILE_IGNORE_NEW_LINES);
$bossHP = explode(": ", $lines[0])[1];
$bossDamage = explode(": ", $lines[1])[1];

$initialState = new GameState(0, 0, $bossHP, $bossDamage, 0, 0, 0, 0);
echo minManaToWin($initialState) . PHP_EOL;
?>
