<?php
$file = file_get_contents("input.txt");
$lines = explode("\n", $file);

$hands = [];
$valueDict = [
    'J' => 1, '2' => 2, '3' => 3, '4' => 4, '5' => 5, '6' => 6, '7' => 7, '8' => 8, '9' => 9, 'T' => 10, 'Q' => 11, 'K' => 12, 'A' => 13
];

foreach ($lines as $line) {
    if (strlen($line) == 0) {
        continue;
    }

    preg_match('/[\dAKQJT]+/', $line, $cards);
    preg_match('/ [\d]+/', $line, $bid);
    $hands[] = ['cards' => $cards[0], 'bid' => (int)substr($bid[0], 1)];
}

$matches = [[], [], [], [], [], [], []];

foreach ($hands as $hand) {
    $count = array_count_values(str_split($hand['cards']));

    if (isset($count['J']) && $count['J'] > 0) {
        $highV = 0;
        $highKey = 'J';
        foreach ($count as $key => $value) {
            if ($key != 'J') {
                if ($value > $highV) {
                    $highKey = $key;
                    $highV = $value;
                } elseif ($value == $highV && $valueDict[$key] > $valueDict[$highKey]) {
                    $highKey = $key;
                }
            }
        }
        if ($highKey != 'J') {
            $count[$highKey] += $count['J'];
            unset($count['J']);
        }
    }

    $value = 1;
    foreach ($count as $c) {
        $value *= $c;
    }

    switch ($value) {
        case 1:
            $matches[6][] = $hand;
            break;
        case 2:
            $matches[5][] = $hand;
            break;
        case 3:
            $matches[3][] = $hand;
            break;
        case 4:
            if (count($count) == 2) {
                $matches[1][] = $hand;
            } else {
                $matches[4][] = $hand;
            }
            break;
        case 5:
            $matches[0][] = $hand;
            break;
        case 6:
            $matches[2][] = $hand;
            break;
        default:
            echo "oh no\n";
            break;
    }
}

$convertedMatches = [];

foreach ($matches as $x) {
    $temp = [];
    foreach ($x as $i) {
        $y = str_replace(['A', 'T', 'J', 'Q', 'K'], ['E', 'A', '1', 'C', 'D'], $i['cards']);
        $val = hexdec($y);
        $temp[] = [$val, $i['bid']];
    }
    usort($temp, function($a, $b) {
        return $b[0] - $a[0];
    });
    $convertedMatches = array_merge($convertedMatches, $temp);
}

$total = 0;
for ($x = 0; $x < count($convertedMatches); $x++) {
    $total += $convertedMatches[$x][1] * (count($convertedMatches) - $x);
}

echo $total . "\n";