
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

for ($i = 0; $i < 5000000; $i++) {
    // Generate next value for A that is a multiple of 4
    do {
        $genA = ($genA * $genAFactor) % $modulus;
    } while ($genA % 4 != 0);

    // Generate next value for B that is a multiple of 8
    do {
        $genB = ($genB * $genBFactor) % $modulus;
    } while ($genB % 8 != 0);

    if (($genA & 0xFFFF) == ($genB & 0xFFFF)) {
        $matches++;
    }
}

echo $matches . "\n";

fclose($file);
?>
