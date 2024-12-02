
<?php

$safeReportCount = 0;
$file = fopen("input.txt", "r");
if ($file === false) {
    die("Failed to open input file");
}

while (($line = fgets($file)) !== false) {
    $levels = array_map('intval', explode(' ', trim($line)));
    if (count($levels) < 2) continue;

    $firstDiff = $levels[1] - $levels[0];
    if ($firstDiff == 0) continue;

    $isIncreasing = $firstDiff > 0;
    $isSafe = true;

    for ($i = 0; $i < count($levels) - 1; $i++) {
        $diff = $levels[$i+1] - $levels[$i];
        if ($diff == 0 || ($isIncreasing && $diff <= 0) || (!$isIncreasing && $diff >= 0) || abs($diff) < 1 || abs($diff) > 3) {
            $isSafe = false;
            break;
        }
    }
    if ($isSafe) $safeReportCount++;
}

fclose($file);
echo $safeReportCount;

?>
