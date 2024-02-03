
<?php

class Ingredient {
    public $name;
    public $capacity;
    public $durability;
    public $flavor;
    public $texture;
    public $calories;

    public function __construct($name, $capacity, $durability, $flavor, $texture, $calories) {
        $this->name = $name;
        $this->capacity = $capacity;
        $this->durability = $durability;
        $this->flavor = $flavor;
        $this->texture = $texture;
        $this->calories = $calories;
    }
}

function readIngredients($filename) {
    $lines = file($filename, FILE_IGNORE_NEW_LINES);
    $ingredients = [];

    foreach ($lines as $line) {
        $parts = explode(" ", $line);
        if (count($parts) < 11) {
            continue; // Invalid line
        }

        $capacity = (int)substr($parts[2], 0, -1);
        $durability = (int)substr($parts[4], 0, -1);
        $flavor = (int)substr($parts[6], 0, -1);
        $texture = (int)substr($parts[8], 0, -1);
        $calories = (int)$parts[10];

        $ingredients[] = new Ingredient($parts[0], $capacity, $durability, $flavor, $texture, $calories);
    }

    return $ingredients;
}

function findMaxScore($ingredients, $totalTeaspoons, $targetCalories) {
    return calculateMaxScore($ingredients, 0, $totalTeaspoons, [], $targetCalories);
}

function calculateMaxScore($ingredients, $index, $remaining, $teaspoons, $targetCalories) {
    if ($index == count($ingredients) - 1) {
        $teaspoons[] = $remaining;
        if (calculateCalories($ingredients, $teaspoons) == $targetCalories) {
            return score($ingredients, $teaspoons);
        }
        return 0;
    }

    $maxScore = 0;
    for ($i = 0; $i <= $remaining; $i++) {
        $score = calculateMaxScore($ingredients, $index + 1, $remaining - $i, array_merge($teaspoons, [$i]), $targetCalories);
        if ($score > $maxScore) {
            $maxScore = $score;
        }
    }
    return $maxScore;
}

function score($ingredients, $teaspoons) {
    $capacity = $durability = $flavor = $texture = 0;
    foreach ($ingredients as $key => $ingredient) {
        $capacity += $ingredient->capacity * $teaspoons[$key];
        $durability += $ingredient->durability * $teaspoons[$key];
        $flavor += $ingredient->flavor * $teaspoons[$key];
        $texture += $ingredient->texture * $teaspoons[$key];
    }

    $capacity = max(0, $capacity);
    $durability = max(0, $durability);
    $flavor = max(0, $flavor);
    $texture = max(0, $texture);

    return $capacity * $durability * $flavor * $texture;
}

function calculateCalories($ingredients, $teaspoons) {
    $calories = 0;
    foreach ($ingredients as $key => $ingredient) {
        $calories += $ingredient->calories * $teaspoons[$key];
    }
    return $calories;
}

$ingredients = readIngredients("input.txt");
$maxScore = findMaxScore($ingredients, 100, 500);
echo $maxScore . PHP_EOL;

?>
