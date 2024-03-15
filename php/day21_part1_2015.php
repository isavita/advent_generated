<?php
$inputFile = 'input.txt';

class Item {
    public $Cost;
    public $Damage;
    public $Armor;

    public function __construct($cost, $damage, $armor) {
        $this->Cost = $cost;
        $this->Damage = $damage;
        $this->Armor = $armor;
    }
}

class Character {
    public $HitPoints;
    public $Damage;
    public $Armor;

    public function __construct($hitPoints, $damage, $armor) {
        $this->HitPoints = $hitPoints;
        $this->Damage = $damage;
        $this->Armor = $armor;
    }
}

function parseStat($line) {
    $parts = explode(': ', $line);
    return intval($parts[1]);
}

function playerWins($player, $boss) {
    $playerDamage = max(1, $player->Damage - $boss->Armor);
    $bossDamage = max(1, $boss->Damage - $player->Armor);

    $playerTurns = ceil($boss->HitPoints / $playerDamage);
    $bossTurns = ceil($player->HitPoints / $bossDamage);

    return $playerTurns <= $bossTurns;
}

$data = file_get_contents($inputFile);
$lines = explode("\n", $data);

$boss = new Character(
    parseStat($lines[0]),
    parseStat($lines[1]),
    parseStat($lines[2])
);

$weapons = [
    new Item(8, 4, 0),
    new Item(10, 5, 0),
    new Item(25, 6, 0),
    new Item(40, 7, 0),
    new Item(74, 8, 0)
];

$armors = [
    new Item(0, 0, 0),
    new Item(13, 0, 1),
    new Item(31, 0, 2),
    new Item(53, 0, 3),
    new Item(75, 0, 4),
    new Item(102, 0, 5)
];

$rings = [
    new Item(0, 0, 0),
    new Item(25, 1, 0),
    new Item(50, 2, 0),
    new Item(100, 3, 0),
    new Item(20, 0, 1),
    new Item(40, 0, 2),
    new Item(80, 0, 3)
];

$minCost = PHP_INT_MAX;
foreach ($weapons as $w) {
    foreach ($armors as $a) {
        for ($ri = 0; $ri < count($rings); $ri++) {
            for ($rj = $ri + 1; $rj < count($rings); $rj++) {
                $player = new Character(100, $w->Damage, $a->Armor);
                $player->Damage += $rings[$ri]->Damage + $rings[$rj]->Damage;
                $player->Armor += $rings[$ri]->Armor + $rings[$rj]->Armor;
                $cost = $w->Cost + $a->Cost + $rings[$ri]->Cost + $rings[$rj]->Cost;
                if (playerWins($player, $boss) && $cost < $minCost) {
                    $minCost = $cost;
                }
            }
        }
    }
}

echo $minCost . "\n";