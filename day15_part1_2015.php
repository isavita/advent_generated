
<?php

class Ingredient {
    public $name;
    public $capacity;
    public $durability;
    public $flavor;
    public $texture;

    public function __construct($name, $capacity, $durability, $flavor, $texture) {
        $this->name = $name;
        $this->capacity = $capacity;
        $this->durability = $durability;
        $this->flavor = $flavor;
        $this->texture = $texture;
    }
}

function readIngredients($filename) {
    $file = fopen($filename, "r");
    $ingredients = [];

    while (!feof($file)) {
        $line = fgets($file);
        $parts = explode(" ", $line);

        if (count($parts) < 11) {
            continue;
        }

        $capacity = intval(substr($parts[2], 0, -1));
        $durability = intval(substr($parts[4], 0, -1));
        $flavor = intval(substr($parts[6], 0, -1));
        $texture = intval(substr($parts[8], 0, -1));

        $ingredients[] = new Ingredient($parts[0], $capacity, $durability, $flavor, $texture);
    }

    fclose($file);
    return $ingredients;
}

function findMaxScore($ingredients, $totalTeaspoons) {
    return calculateMaxScore($ingredients, 0, $totalTeaspoons, []);
}

function calculateMaxScore($ingredients, $index, $remaining, $teaspoons) {
    if ($index == count($ingredients) - 1) {
        $teaspoons[] = $remaining;
        return score($ingredients, $teaspoons);
    }

    $maxScore = 0;
    for ($i = 0; $i <= $remaining; $i++) {
        $score = calculateMaxScore($ingredients, $index + 1, $remaining - $i, array_merge($teaspoons, [$i]));
        if ($score > $maxScore) {
            $maxScore = $score;
        }
    }
    return $maxScore;
}

function score($ingredients, $teaspoons) {
    $capacity = $durability = $flavor = $texture = 0;

    foreach ($ingredients as $index => $ingredient) {
        $capacity += $ingredient->capacity * $teaspoons[$index];
        $durability += $ingredient->durability * $teaspoons[$index];
        $flavor += $ingredient->flavor * $teaspoons[$index];
        $texture += $ingredient->texture * $teaspoons[$index];
    }

    $capacity = max(0, $capacity);
    $durability = max(0, $durability);
    $flavor = max(0, $flavor);
    $texture = max(0, $texture);

    return $capacity * $durability * $flavor * $texture;
}

$ingredients = readIngredients("input.txt");
$maxScore = findMaxScore($ingredients, 100);
echo $maxScore . PHP_EOL;

?>
