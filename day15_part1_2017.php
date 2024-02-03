
<?php

$file = fopen("input.txt", "r");
$genAStart = intval(fgets($file));
$genBStart = intval(fgets($file));

$genAFactor = 16807;
$genBFactor = 48271;
$modulus = 2147483647;

$genA = $genAStart;
$genB = $genBStart;
$matches = 0;

for ($i = 0; $i < 40000000; $i++) {
    $genA = ($genA * $genAFactor) % $modulus;
    $genB = ($genB * $genBFactor) % $modulus;

    if (($genA & 0xFFFF) == ($genB & 0xFFFF)) {
        $matches++;
    }
}

echo $matches;

fclose($file);
?>
