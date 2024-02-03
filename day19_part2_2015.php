
<?php

$input = file_get_contents("input.txt");
echo solve($input);

function parseInput($input) {
    $blocks = explode("\n\n", $input);
    $startingMaterial = splitMolecules($blocks[1]);

    $graph = [];

    foreach (explode("\n", $blocks[0]) as $l) {
        $parts = explode(" => ", $l);
        $graph[$parts[0]][] = $parts[1];
    }

    return [$graph, $startingMaterial];
}

function splitMolecules($input) {
    $molecules = [];
    $chars = str_split($input);
    $prevCode = null;
    $currentMolecule = "";

    foreach ($chars as $char) {
        $code = ord($char);
        if ($code >= 65 && $code <= 90) {
            if ($currentMolecule !== "") {
                $molecules[] = $currentMolecule;
            }
            $currentMolecule = $char;
        } else {
            $currentMolecule .= $char;
        }
    }

    $molecules[] = $currentMolecule;

    return $molecules;
}

function solve($input) {
    [$reverseGraph, $startingMols] = parseInput($input);

    $productToReactant = [];
    foreach ($reverseGraph as $react => $products) {
        foreach ($products as $p) {
            if (array_key_exists($p, $productToReactant)) {
                throw new Exception("dup found");
            }
            $productToReactant[$p] = $react;
        }
    }

    $allProducts = array_keys($productToReactant);

    $start = implode("", $startingMols);
    $mol = $start;

    $steps = 0;
    while ($mol !== "e") {
        $changeMade = false;
        foreach ($allProducts as $prod) {
            $count = substr_count($mol, $prod);
            if ($count <= 0) {
                continue;
            }
            $changeMade = true;
            $steps += $count;
            $mol = str_replace($prod, $productToReactant[$prod], $mol);
            break;
        }

        if (!$changeMade) {
            shuffle($allProducts);
            $mol = $start;
            $steps = 0;
        }
    }

    return $steps;
}
