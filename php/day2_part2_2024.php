
<?php

$safeReportCount = 0;

$file = fopen("input.txt", "r");
if ($file === false) {
    die("Failed to open input file");
}

while (($line = fgets($file)) !== false) {
    $levels = array_map('intval', explode(' ', trim($line)));
    if (isSafeReport($levels) || isSafeWithOneRemoval($levels)) {
        $safeReportCount++;
    }
}

fclose($file);
echo $safeReportCount . PHP_EOL;


function isSafeReport(array $levels): bool
{
    if (count($levels) < 2) {
        return false;
    }

    $firstDiff = $levels[1] - $levels[0];
    if ($firstDiff === 0) {
        return false;
    }

    $isIncreasing = $firstDiff > 0;

    for ($i = 0; $i < count($levels) - 1; $i++) {
        $diff = $levels[$i + 1] - $levels[$i];
        if ($diff === 0 || ($isIncreasing && $diff <= 0) || (!$isIncreasing && $diff >= 0) || abs($diff) < 1 || abs($diff) > 3) {
            return false;
        }
    }

    return true;
}

function isSafeWithOneRemoval(array $levels): bool
{
    for ($i = 0; $i < count($levels); $i++) {
        $modifiedLevels = array_merge(array_slice($levels, 0, $i), array_slice($levels, $i + 1));
        if (isSafeReport($modifiedLevels)) {
            return true;
        }
    }
    return false;
}

?>
