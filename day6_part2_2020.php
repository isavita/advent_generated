
<?php

$file = fopen("input.txt", "r");
$totalCount = 0;
$groupAnswers = [];
$groupSize = 0;

while (($line = fgets($file)) !== false) {
    $line = trim($line);
    if ($line == "") {
        foreach ($groupAnswers as $count) {
            if ($count == $groupSize) {
                $totalCount++;
            }
        }
        $groupAnswers = [];
        $groupSize = 0;
    } else {
        $groupSize++;
        $chars = str_split($line);
        foreach ($chars as $char) {
            if (array_key_exists($char, $groupAnswers)) {
                $groupAnswers[$char]++;
            } else {
                $groupAnswers[$char] = 1;
            }
        }
    }
}

foreach ($groupAnswers as $count) {
    if ($count == $groupSize) {
        $totalCount++;
    }
}

echo $totalCount . PHP_EOL;

fclose($file);
?>
