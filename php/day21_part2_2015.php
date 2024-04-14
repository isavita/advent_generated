<?php

class Item {
    public $cost;
    public $damage;
    public $armor;

    public function __construct($cost, $damage = 0, $armor = 0) {
        $this->cost = $cost;
        $this->damage = $damage;
        $this->armor = $armor;
    }
}

class Character {
    public $hitPoints;
    public $damage;
    public $armor;

    public function __construct($hitPoints, $damage, $armor) {
        $this->hitPoints = $hitPoints;
        $this->damage = $damage;
        $this->armor = $armor;
    }
}

function parseStat($line) {
    $parts = explode(": ", $line);
    return intval($parts[1]);
}

function playerWins($player, $boss) {
    $playerDamage = max(1, $player->damage - $boss->armor);
    $bossDamage = max(1, $boss->damage - $player->armor);

    $playerTurns = (int) ceil($boss->hitPoints / $playerDamage);
    $bossTurns = (int) ceil($player->hitPoints / $bossDamage);

    return $playerTurns <= $bossTurns;
}

$data = file_get_contents("input.txt");
$lines = explode("\n", $data);
$boss = new Character(parseStat($lines[0]), parseStat($lines[1]), parseStat($lines[2]));

$weapons = [
    new Item(8, 4),
    new Item(10, 5),
    new Item(25, 6),
    new Item(40, 7),
    new Item(74, 8),
];

$armors = [
    new Item(0, 0, 0),
    new Item(13, 0, 1),
    new Item(31, 0, 2),
    new Item(53, 0, 3),
    new Item(75, 0, 4),
    new Item(102, 0, 5),
];

$rings = [
    new Item(0),
    new Item(25, 1),
    new Item(50, 2),
    new Item(100, 3),
    new Item(20, 0, 1),
    new Item(40, 0, 2),
    new Item(80, 0, 3),
];

$maxCost = 0;
foreach ($weapons as $w) {
    foreach ($armors as $a) {
        for ($ri = 0; $ri < count($rings); $ri++) {
            for ($rj = $ri + 1; $rj < count($rings); $rj++) {
                $player = new Character(100, $w->damage + $rings[$ri]->damage + $rings[$rj]->damage, $a->armor + $rings[$ri]->armor + $rings[$rj]->armor);
                $cost = $w->cost + $a->cost + $rings[$ri]->cost + $rings[$rj]->cost;
                if (!playerWins($player, $boss) && $cost > $maxCost) {
                    $maxCost = $cost;
                }
            }
        }
    }
}

echo $maxCost . "\n";

?>