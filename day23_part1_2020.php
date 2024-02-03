
<?php

$file = fopen("input.txt", "r");
$input = fgets($file);
fclose($file);

$cups = array_fill(0, strlen($input) + 1, 0);
$currentCup = 0;

for ($i = 0; $i < strlen($input); $i++) {
    $cup = (int)$input[$i];
    if ($i == 0) {
        $currentCup = $cup;
    }
    if ($i < strlen($input) - 1) {
        $nextCup = (int)$input[$i + 1];
        $cups[$cup] = $nextCup;
    }
}

$firstCup = (int)$input[0];
$lastCup = (int)$input[strlen($input) - 1];
$cups[$lastCup] = $firstCup;

for ($i = 0; $i < 100; $i++) {
    $pickup1 = $cups[$currentCup];
    $pickup2 = $cups[$pickup1];
    $pickup3 = $cups[$pickup2];

    $cups[$currentCup] = $cups[$pickup3];

    $destinationCup = $currentCup - 1;
    if ($destinationCup < 1) {
        $destinationCup = strlen($input);
    }
    while ($destinationCup == $pickup1 || $destinationCup == $pickup2 || $destinationCup == $pickup3) {
        $destinationCup--;
        if ($destinationCup < 1) {
            $destinationCup = strlen($input);
        }
    }

    $cups[$pickup3] = $cups[$destinationCup];
    $cups[$destinationCup] = $pickup1;

    $currentCup = $cups[$currentCup];
}

$cup = $cups[1];
while ($cup != 1) {
    echo $cup;
    $cup = $cups[$cup];
    if ($cup == 1) {
        break;
    }
}
echo "\n";
?>
