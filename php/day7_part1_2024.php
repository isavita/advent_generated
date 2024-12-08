
<?php

function canProduce(int $target, array $nums, int $idx, int $current): bool {
    if ($idx == count($nums)) {
        return $current == $target;
    }
    return canProduce($target, $nums, $idx + 1, $current + $nums[$idx]) || canProduce($target, $nums, $idx + 1, $current * $nums[$idx]);
}

$file = fopen("input.txt", "r");
$total = 0;
while (($line = fgets($file)) !== false) {
    $line = trim($line);
    if (empty($line)) continue;
    $parts = explode(":", $line);
    $target = (int)trim($parts[0]);
    $numStrs = explode(" ", trim($parts[1]));
    $nums = array_map('intval', $numStrs);

    if (count($nums) == 1) {
        if ($nums[0] == $target) {
            $total += $target;
        }
        continue;
    }
    if (canProduce($target, $nums, 1, $nums[0])) {
        $total += $target;
    }
}
fclose($file);
echo $total . PHP_EOL;

?>
