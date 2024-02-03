
<?php

$input = file_get_contents("input.txt");
$numbers = array_map('intval', explode("\n", $input));

$preambleLength = 25;

for ($i = $preambleLength; $i < count($numbers); $i++) {
    if (!isValid($numbers[$i], array_slice($numbers, $i - $preambleLength, $preambleLength))) {
        echo $numbers[$i] . "\n";
        break;
    }
}

function isValid($number, $previousNumbers) {
    $seen = [];
    foreach ($previousNumbers as $n) {
        if (isset($seen[$number - $n])) {
            return true;
        }
        $seen[$n] = true;
    }
    return false;
}
?>
