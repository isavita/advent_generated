
<?php

$file = fopen("input.txt", "r");
if ($file === false) {
    echo "Error opening file\n";
    return;
}

$totalScore = 0;

while (($line = fgets($file)) !== false) {
    $opponent = $line[0];
    $yourMove = $line[2];

    $score = 0;
    if ($yourMove == 'X') {
        $score = 1;
    } elseif ($yourMove == 'Y') {
        $score = 2;
    } elseif ($yourMove == 'Z') {
        $score = 3;
    }

    if (($opponent == 'A' && $yourMove == 'Y') || ($opponent == 'B' && $yourMove == 'Z') || ($opponent == 'C' && $yourMove == 'X')) {
        $score += 6;
    } elseif ($opponent == 'A' && $yourMove == 'X' || $opponent == 'B' && $yourMove == 'Y' || $opponent == 'C' && $yourMove == 'Z') {
        $score += 3;
    }

    $totalScore += $score;
}

fclose($file);

echo $totalScore . "\n";
?>
