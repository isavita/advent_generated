
<?php

function concat(int $a, int $b): int {
    return (int)("$a$b");
}

function canProduce(int $target, array $nums, int $idx, int $value): bool {
    if ($idx == count($nums)) {
        return $value == $target;
    }
    $n = $nums[$idx];
    if (canProduce($target, $nums, $idx + 1, $value + $n)) return true;
    if (canProduce($target, $nums, $idx + 1, $value * $n)) return true;
    if (canProduce($target, $nums, $idx + 1, concat($value, $n))) return true;
    return false;
}

$lines = file('input.txt', FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
$total = 0;
foreach ($lines as $line) {
    [$targetStr, $numsStr] = explode(":", $line);
    $target = (int)trim($targetStr);
    $numsStr = trim($numsStr);
    if(empty($numsStr)) continue;
    $nums = array_map('intval', explode(" ", $numsStr));

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
echo $total;

?>
