
<?php

$input = file_get_contents("input.txt");
$lines = explode("\n", $input);

$allergenMap = [];
$ingredientCount = [];
$safeIngredients = [];

foreach ($lines as $line) {
    $parts = explode(" (contains ", $line);
    $ingredients = explode(" ", $parts[0]);
    $allergens = [];
    if (count($parts) > 1) {
        $allergens = explode(", ", substr($parts[1], 0, -1));
    }

    foreach ($ingredients as $ingredient) {
        if (!isset($ingredientCount[$ingredient])) {
            $ingredientCount[$ingredient] = 1;
            $safeIngredients[$ingredient] = true;
        } else {
            $ingredientCount[$ingredient]++;
        }
    }

    foreach ($allergens as $allergen) {
        if (!isset($allergenMap[$allergen])) {
            $allergenMap[$allergen] = array_fill_keys($ingredients, true);
        } else {
            foreach (array_keys($allergenMap[$allergen]) as $ing) {
                if (!in_array($ing, $ingredients)) {
                    unset($allergenMap[$allergen][$ing]);
                }
            }
        }
    }
}

foreach ($allergenMap as $ingredients) {
    foreach (array_keys($ingredients) as $ingredient) {
        unset($safeIngredients[$ingredient]);
    }
}

$count = 0;
foreach (array_keys($safeIngredients) as $ingredient) {
    $count += $ingredientCount[$ingredient];
}

echo $count . PHP_EOL;
?>
