
<?php
$lines = file('input.txt', FILE_IGNORE_NEW_LINES);
$parsing = true;
$ranges = [];
$ids = [];

foreach ($lines as $line) {
    $s = trim($line);
    if ($s === '') {
        $parsing = false;
        continue;
    }
    if ($parsing) {
        [$a, $b] = array_map('trim', explode('-', $s, 2));
        $ranges[] = ['min' => (int)$a, 'max' => (int)$b];
    } else {
        $ids[] = (int)$s;
    }
}

usort($ranges, fn($x, $y) => $x['min'] <=> $y['min'] ?: $x['max'] <=> $y['max']);

$merged = [];
foreach ($ranges as $r) {
    if (!$merged || $r['min'] > $merged[count($merged) - 1]['max']) {
        $merged[] = $r;
    } elseif ($r['max'] > $merged[count($merged) - 1]['max']) {
        $merged[count($merged) - 1]['max'] = $r['max'];
    }
}

function contains(array $arr, int $x): bool
{
    $l = 0;
    $r = count($arr);
    while ($l < $r) {
        $m = ($l + $r) >> 1;
        if ($x < $arr[$m]['min']) {
            $r = $m;
        } elseif ($x > $arr[$m]['max']) {
            $l = $m + 1;
        } else {
            return true;
        }
    }
    return false;
}

$fresh = 0;
foreach ($ids as $id) {
    if ($merged && contains($merged, $id)) {
        $fresh++;
    }
}

echo "Number of fresh ingredients: {$fresh}\n";
