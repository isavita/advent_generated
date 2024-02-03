
<?php

$input = file_get_contents("input.txt");
$lines = explode("\n", $input);

$allergenMap = [];
$ingredientAllergen = [];

foreach ($lines as $line) {
    $parts = explode(" (contains ", $line);
    $ingredients = explode(" ", $parts[0]);
    $allergens = [];
    if (count($parts) > 1) {
        $allergens = explode(", ", substr($parts[1], 0, -1));
    }

    foreach ($allergens as $allergen) {
        if (!array_key_exists($allergen, $allergenMap)) {
            $allergenMap[$allergen] = array_fill_keys($ingredients, true);
        } else {
            foreach (array_keys($allergenMap[$allergen]) as $ingredient) {
                if (!in_array($ingredient, $ingredients)) {
                    unset($allergenMap[$allergen][$ingredient]);
                }
            }
        }
    }
}

while (count($allergenMap) > 0) {
    foreach ($allergenMap as $allergen => $ingredients) {
        if (count($ingredients) == 1) {
            $ingredient = array_keys($ingredients)[0];
            $ingredientAllergen[$allergen] = $ingredient;
            foreach ($allergenMap as &$ing) {
                unset($ing[$ingredient]);
            }
            unset($allergenMap[$allergen]);
        }
    }
}

ksort($ingredientAllergen);
echo implode(",", $ingredientAllergen) . "\n";
?>
