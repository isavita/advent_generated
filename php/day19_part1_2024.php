
<?php
$patterns_str = trim(fgets(fopen("input.txt", "r")));
$patterns = array_map('trim', explode(',', $patterns_str));
fgets(fopen("input.txt", "r"));

$count = 0;
$file = fopen("input.txt", "r");
fgetcsv($file, 0);
fgetcsv($file, 0);
while ($design = fgets($file)) {
    if (canMake(trim($design), $patterns)) {
        $count++;
    }
}
fclose($file);
echo $count . "\n";

function canMake(string $design, array $patterns): bool {
    $n = strlen($design);
    $dp = array_fill(0, $n + 1, false);
    $dp[0] = true;
    for ($i = 1; $i <= $n; $i++) {
        foreach ($patterns as $p) {
            $lp = strlen($p);
            if ($i >= $lp && $dp[$i - $lp] && substr($design, $i - $lp, $lp) === $p) {
                $dp[$i] = true;
                break;
            }
        }
    }
    return $dp[$n];
}
?>
