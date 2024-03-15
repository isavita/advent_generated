<?php

class Chemical {
    public $name;
    public $amount;

    public function __construct($name, $amount) {
        $this->name = $name;
        $this->amount = $amount;
    }
}

function parseChemical($s) {
    $parts = explode(" ", $s);
    return new Chemical($parts[1], intval($parts[0]));
}

function calculateOre($chem, $amount, $reactions, $ingredients, &$surplus) {
    if ($chem == "ORE") {
        return $amount;
    }

    if (isset($surplus[$chem]) && $surplus[$chem] >= $amount) {
        $surplus[$chem] -= $amount;
        return 0;
    }

    $amount -= isset($surplus[$chem]) ? $surplus[$chem] : 0;
    $surplus[$chem] = 0;
    $reaction = $reactions[$chem];
    $times = (int)(($amount + $reaction->amount - 1) / $reaction->amount);
    $ore = 0;

    foreach ($ingredients[$chem] as $ingredient) {
        $ore += calculateOre($ingredient->name, $ingredient->amount * $times, $reactions, $ingredients, $surplus);
    }

    $surplus[$chem] = $times * $reaction->amount - $amount;
    return $ore;
}

$file = fopen("input.txt", "r");
$reactions = [];
$ingredients = [];

while (($line = fgets($file)) !== false) {
    $parts = explode(" => ", trim($line));
    $output = parseChemical($parts[1]);
    $inputs = [];
    foreach (explode(", ", $parts[0]) as $input) {
        $inputs[] = parseChemical($input);
    }
    $reactions[$output->name] = $output;
    $ingredients[$output->name] = $inputs;
}

fclose($file);

$surplus = [];
echo calculateOre("FUEL", 1, $reactions, $ingredients, $surplus);