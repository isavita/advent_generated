
<?php

function next_secret(int $s): int
{
    $mod = 1 << 24;
    $s ^= ($s * 64) & ($mod - 1);
    $s ^= intdiv($s, 32);
    $s ^= ($s * 2048) & ($mod - 1);
    return $s & ($mod - 1);
}

function encode_change4(int $c1, int $c2, int $c3, int $c4): int
{
    return ($c1 + 9) + ($c2 + 9) * 19 + ($c3 + 9) * 361 + ($c4 + 9) * 6859;
}

function solve(): void
{
    $num_steps = 2000;
    $pattern_count = 19 ** 4;

    $initials = array_map('intval', file('input.txt', FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES));

    $global_sum = array_fill(0, $pattern_count, 0);

    foreach ($initials as $init_val) {
        $s = $init_val;
        $prices = [];
        for ($i = 0; $i <= $num_steps; ++$i) {
            $prices[] = $s % 10;
            $s = next_secret($s);
        }

        $local_price = array_fill(0, $pattern_count, -1);
        for ($i = 0; $i < $num_steps - 3; ++$i) {
            $changes = [];
            for ($j = 0; $j < 4; ++$j) {
                $changes[$j] = $prices[$i + $j + 1] - $prices[$i + $j];
            }

            if (min($changes) >= -9 && max($changes) <= 9) {
                $idx = encode_change4($changes[0], $changes[1], $changes[2], $changes[3]);
                if ($local_price[$idx] < 0) {
                    $local_price[$idx] = $prices[$i + 4];
                }
            }
        }

        foreach ($local_price as $idx => $p) {
            if ($p >= 0) {
                $global_sum[$idx] += $p;
            }
        }
    }

    echo max($global_sum);
}

solve();
?>
