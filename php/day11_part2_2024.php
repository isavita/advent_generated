
<?php

function trim_leading_zeros(string $s): string {
    $i = 0;
    while ($i < strlen($s) - 1 && $s[$i] == '0') {
        $i++;
    }
    return substr($s, $i);
}

function split_stone(string $s): array {
    $mid = strlen($s) >> 1;
    $left = trim_leading_zeros(substr($s, 0, $mid));
    $right = trim_leading_zeros(substr($s, $mid));
    return [$left ?: "0", $right ?: "0"];
}

function multiply_by_2024(string $s): string {
    $num = array_map('intval', str_split($s));
    $multiplier = [2, 0, 2, 4];
    $result = array_fill(0, count($num) + count($multiplier), 0);

    for ($i = count($num) - 1; $i >= 0; $i--) {
        $carry = 0;
        for ($j = count($multiplier) - 1; $j >= 0; $j--) {
            $product = $num[$i] * $multiplier[$j] + $result[$i + $j + 1] + $carry;
            $result[$i + $j + 1] = $product % 10;
            $carry = intdiv($product, 10);
        }
        $result[$i] += $carry;
    }

    $start = 0;
    while ($start < count($result) - 1 && $result[$start] == 0) {
        $start++;
    }

    return implode('', array_slice($result, $start));
}

function solve(): void {
    $filename = "input.txt";
    if (!file_exists($filename)) {
        echo "Error: $filename not found.\n";
        exit(1);
    }

    $line = trim(file_get_contents($filename));
    $stones = explode(" ", $line);
    $stones_map = array_count_values($stones);

    $steps = 75;
    for ($i = 0; $i < $steps; $i++) {
        $new_stones_map = [];
        foreach ($stones_map as $stone => $count) {
            if ($stone == "0") {
                $new_stones_map["1"] = ($new_stones_map["1"] ?? 0) + $count;
            } elseif (strlen($stone) % 2 == 0) {
                [$left, $right] = split_stone($stone);
                $new_stones_map[$left] = ($new_stones_map[$left] ?? 0) + $count;
                $new_stones_map[$right] = ($new_stones_map[$right] ?? 0) + $count;
            } else {
                $new_stone = multiply_by_2024($stone);
                $new_stones_map[$new_stone] = ($new_stones_map[$new_stone] ?? 0) + $count;
            }
        }
        $stones_map = $new_stones_map;
    }

    $total_stones = array_sum($stones_map);
    echo $total_stones . "\n";
}

solve();
