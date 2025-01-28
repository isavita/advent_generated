
<?php
function findPosition(array $mat, string $ch): array {
    foreach ($mat as $i => $row) {
        foreach (str_split($row) as $j => $c) {
            if ($c === $ch) {
                return ['i' => $i, 'j' => $j];
            }
        }
    }
    return ['i' => -1, 'j' => -1];
}

function ok(array $mat, array $st, string $seq): bool {
    $curr = $st;
    foreach (str_split($seq) as $ch) {
        if ($mat[$curr['i']][$curr['j']] === ' ') {
            return false;
        }
        switch ($ch) {
            case '^':
                $curr['i']--;
                break;
            case 'v':
                $curr['i']++;
                break;
            case '<':
                $curr['j']--;
                break;
            case '>':
                $curr['j']++;
                break;
        }

        if ($curr['i'] < 0 || $curr['i'] >= count($mat) || $curr['j'] < 0 || $curr['j'] >= strlen($mat[0])) {
            return false;
        }
    }
    return true;
}

function generateMoves(array $position, string $objective, array $pad): string {
    $objPos = findPosition($pad, $objective);
    $ret = '';

    if ($position['j'] > $objPos['j']) {
        $ret .= str_repeat('<', $position['j'] - $objPos['j']);
    }
    if ($position['i'] > $objPos['i']) {
        $ret .= str_repeat('^', $position['i'] - $objPos['i']);
    }
    if ($position['i'] < $objPos['i']) {
        $ret .= str_repeat('v', $objPos['i'] - $position['i']);
    }
    if ($position['j'] < $objPos['j']) {
        $ret .= str_repeat('>', $objPos['j'] - $position['j']);
    }

    if (!ok($pad, $position, $ret)) {
        $ret = '';
        if ($position['j'] < $objPos['j']) {
            $ret .= str_repeat('>', $objPos['j'] - $position['j']);
        }
        if ($position['i'] > $objPos['i']) {
            $ret .= str_repeat('^', $position['i'] - $objPos['i']);
        }
        if ($position['i'] < $objPos['i']) {
            $ret .= str_repeat('v', $objPos['i'] - $position['i']);
        }
        if ($position['j'] > $objPos['j']) {
            $ret .= str_repeat('<', $position['j'] - $objPos['j']);
        }
    }

    return $ret;
}

function solve(string $code, int $robots, array $keyPad, array $robotPad, int $maxRobots): int {
    if ($robots <= 0) {
        return strlen($code);
    }

    $ret = 0;
    $posi = 3;
    $posj = 2;
    if ($robots !== $maxRobots) {
        $posi = 0;
    }

    $moves = '';
    for ($i = 0; $i < strlen($code); $i++) {
        $ch = $code[$i];
        if ($robots === $maxRobots) {
            $moves = generateMoves(['i' => $posi, 'j' => $posj], $ch, $keyPad);
            $pos = findPosition($keyPad, $ch);
            $posi = $pos['i'];
            $posj = $pos['j'];
        } else {
            $moves = generateMoves(['i' => $posi, 'j' => $posj], $ch, $robotPad);
            $pos = findPosition($robotPad, $ch);
            $posi = $pos['i'];
            $posj = $pos['j'];
        }
        $ret += solve($moves . 'A', $robots - 1, $keyPad, $robotPad, $maxRobots);
    }
    return $ret;
}

$maxRobots = 3;
$keyPad = [
    "789",
    "456",
    "123",
    " 0A",
];
$robotPad = [
    " ^A",
    "<v>",
];

$ret = 0;
$content = trim(file_get_contents("input.txt"));
$codes = explode("\n", $content);

foreach ($codes as $codeLine) {
    $codeLine = trim($codeLine);
    if ($codeLine === "") {
        continue;
    }

    $numericPart = 0;
    for ($i = 0; $i < strlen($codeLine); $i++) {
        if (is_numeric($codeLine[$i])) {
            $numericPart = $numericPart * 10 + (int)$codeLine[$i];
        }
    }

    $sv = solve($codeLine, $maxRobots, $keyPad, $robotPad, $maxRobots);
    $ret += $sv * $numericPart;
}

echo $ret . PHP_EOL;
?>
