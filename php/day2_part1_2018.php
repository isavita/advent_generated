
<?php

$file = fopen("input.txt", "r") or die("Unable to open file!");
$twoCount = 0;
$threeCount = 0;

while(!feof($file)) {
    $id = trim(fgets($file));
    list($twos, $threes) = countTwosAndThrees($id);
    if ($twos) {
        $twoCount++;
    }
    if ($threes) {
        $threeCount++;
    }
}

fclose($file);

$checksum = $twoCount * $threeCount;
echo $checksum . "\n";

function countTwosAndThrees($id) {
    $charCount = array_count_values(str_split($id));
    $hasTwos = false;
    $hasThrees = false;

    foreach ($charCount as $count) {
        if ($count == 2) {
            $hasTwos = true;
        } elseif ($count == 3) {
            $hasThrees = true;
        }
    }

    return array($hasTwos, $hasThrees);
}
?>
