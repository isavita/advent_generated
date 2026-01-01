
<?php

ini_set('memory_limit', '-1');

function main() {
    if (!file_exists("input.txt")) return;

    $pts = [];
    foreach (file("input.txt", FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES) as $l) {
        $p = array_map('intval', explode(',', trim($l)));
        if (count($p) === 3) $pts[] = $p;
    }

    $n = count($pts);
    if ($n < 2) return;

    $e = [];
    for ($i = 0; $i < $n; $i++) {
        for ($j = $i + 1; $j < $n; $j++) {
            $dx = $pts[$i][0] - $pts[$j][0];
            $dy = $pts[$i][1] - $pts[$j][1];
            $dz = $pts[$i][2] - $pts[$j][2];
            $e[] = [$i, $j, $dx * $dx + $dy * $dy + $dz * $dz];
        }
    }

    usort($e, fn($a, $b) => $a[2] <=> $b[2]);

    $par = range(0, $n - 1);
    $rnk = array_fill(0, $n, 0);
    $find = function ($i) use (&$par, &$find) {
        return $par[$i] === $i ? $i : ($par[$i] = $find($par[$i]));
    };

    $c = $n;
    foreach ($e as [$u, $v, $d]) {
        $ru = $find($u);
        $rv = $find($v);
        if ($ru !== $rv) {
            if ($rnk[$ru] < $rnk[$rv]) {
                $par[$ru] = $rv;
            } elseif ($rnk[$ru] > $rnk[$rv]) {
                $par[$rv] = $ru;
            } else {
                $par[$rv] = $ru;
                $rnk[$ru]++;
            }

            if (--$c === 1) {
                $p1 = $pts[$u];
                $p2 = $pts[$v];
                echo "Connected {$p1[0]},{$p1[1]},{$p1[2]} and {$p2[0]},{$p2[1]},{$p2[2]}\n";
                echo "Product of X coordinates: " . ($p1[0] * $p2[0]) . "\n";
                break;
            }
        }
    }
}

main();
