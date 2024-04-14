<?php
$input = file_get_contents("input.txt");
echo firewall($input);

function firewall($input) {
    $allBlockedRanges = [];
    $lines = explode("\n", trim($input));
    foreach ($lines as $line) {
        sscanf($line, "%d-%d", $start, $end);
        $allBlockedRanges[] = [$start, $end];
    }
    usort($allBlockedRanges, function ($a, $b) {
        return $a[0] <=> $b[0] ?: $a[1] <=> $b[1];
    });

    // Merge ranges
    $merged = [];
    foreach ($allBlockedRanges as $range) {
        if (empty($merged) || $merged[count($merged) - 1][1] < $range[0] - 1) {
            $merged[] = $range;
        } else {
            $merged[count($merged) - 1][1] = max($merged[count($merged) - 1][1], $range[1]);
        }
    }

    // Calculate allowed IPs
    $totalAllowed = 0;
    for ($i = 1; $i < count($merged); $i++) {
        $totalAllowed += $merged[$i][0] - $merged[$i - 1][1] - 1;
    }

    return $totalAllowed;
}
?>