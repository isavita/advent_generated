
<?php
/*  ---------- 128‑bit helpers using GMP ----------  */
function ceil_div(gmp $a, gmp $b): gmp
{
    $q = gmp_div_q($a, $b);
    $r = gmp_mod($a, $b);
    return gmp_cmp($r, 0) === 0 ? $q : gmp_add($q, 1);
}

/*  ---------- read input ----------  */
$txt = file_get_contents('input.txt');
$txt = preg_replace('/[\s\r\n]+/', ',', $txt);
$parts = array_filter(explode(',', $txt), 'strlen');

/*  ---------- pre‑compute powers of 10 ----------  */
$pow10 = [];
$pow10[0] = gmp_init(1);
for ($i = 1; $i <= 20; $i++) {
    $pow10[$i] = gmp_mul($pow10[$i - 1], 10);
}

/*  ---------- main logic ----------  */
$ids = [];

foreach ($parts as $rng) {
    [$l, $r] = explode('-', $rng, 2);
    $start = gmp_init($l);
    $end   = gmp_init($r);
    if (gmp_cmp($start, $end) > 0) {
        $tmp   = $start;
        $start = $end;
        $end   = $tmp;
    }

    for ($k = 1; $k <= 10; $k++) {
        $multiplier = gmp_add($pow10[$k], 1);          // 10^k + 1
        $minSeed    = $pow10[$k - 1];                  // 10^{k‑1}
        $maxSeed    = gmp_sub($pow10[$k], 1);          // 10^k – 1

        $sMin = ceil_div($start, $multiplier);
        $sMax = gmp_div_q($end, $multiplier);

        if (gmp_cmp($sMin, $minSeed) < 0) $sMin = $minSeed;
        if (gmp_cmp($sMax, $maxSeed) > 0) $sMax = $maxSeed;
        if (gmp_cmp($sMin, $sMax) > 0) continue;

        for ($seed = $sMin; gmp_cmp($seed, $sMax) <= 0; $seed = gmp_add($seed, 1)) {
            $ids[] = gmp_strval(gmp_mul($seed, $multiplier));
        }
    }
}

/*  ---------- sort, deduplicate, sum ----------  */
usort($ids, function ($a, $b) {
    $la = strlen($a);
    $lb = strlen($b);
    if ($la !== $lb) return $la <=> $lb;
    return strcmp($a, $b);
});

$sum  = gmp_init(0);
$prev = null;
foreach ($ids as $id) {
    if ($id !== $prev) {
        $sum = gmp_add($sum, $id);
        $prev = $id;
    }
}

echo gmp_strval($sum), PHP_EOL;
?>
