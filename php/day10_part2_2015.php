
<?php

$initialSequence = trim(file_get_contents("input.txt"));
$result = lookAndSay($initialSequence, 50);
echo strlen($result) . PHP_EOL;

function lookAndSay($sequence, $iterations) {
    for ($i = 0; $i < $iterations; $i++) {
        $sequence = nextSequence($sequence);
    }
    return $sequence;
}

function nextSequence($sequence) {
    $result = "";
    $i = 0;
    while ($i < strlen($sequence)) {
        $count = 1;
        $digit = $sequence[$i];
        for ($j = $i + 1; $j < strlen($sequence) && $sequence[$j] == $digit; $j++) {
            $count++;
        }
        $result .= $count . $digit;
        $i += $count;
    }
    return $result;
}
?>
