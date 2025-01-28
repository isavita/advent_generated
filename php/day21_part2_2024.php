
<?php
function findPosition(array $mat, string $ch): array {
    static $positionCache = [];
    $key = $ch . implode("", $mat);
    if (isset($positionCache[$key])) {
        return $positionCache[$key];
    }

    foreach ($mat as $i => $row) {
        for ($j = 0; $j < strlen($row); $j++) {
            if ($row[$j] === $ch) {
                $pos = ['i' => $i, 'j' => $j];
                $positionCache[$key] = $pos;
                return $pos;
            }
        }
    }
    return ['i' => -1, 'j' => -1];
}

function ok(array $mat, array $st, string $seq): bool {
    static $okCache = [];
    $key = $st['i'] . "," . $st['j'] . "," . $seq . "," . implode("", $mat);
    if (isset($okCache[$key])) {
        return $okCache[$key];
    }

    $curr = $st;
    for ($i = 0; $i < strlen($seq); $i++) {
        if ($mat[$curr['i']][$curr['j']] === ' ') {
            $okCache[$key] = false;
            return false;
        }

        $ch = $seq[$i];
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
            $okCache[$key] = false;
            return false;
        }
    }

    $okCache[$key] = true;
    return true;
}

function generateMoves(array $position, string $objective, array $pad): string {
    static $moveCache = [];
    $key = serialize([$position, $objective, $pad]);
    if (isset($moveCache[$key])) {
        return $moveCache[$key];
    }

    $objPos = findPosition($pad, $objective);

    $ret = "";
    if ($position['j'] > $objPos['j']) {
        $ret .= str_repeat("<", $position['j'] - $objPos['j']);
    }
    if ($position['i'] > $objPos['i']) {
        $ret .= str_repeat("^", $position['i'] - $objPos['i']);
    }
    if ($position['i'] < $objPos['i']) {
        $ret .= str_repeat("v", $objPos['i'] - $position['i']);
    }
    if ($position['j'] < $objPos['j']) {
        $ret .= str_repeat(">", $objPos['j'] - $position['j']);
    }

    if (!ok($pad, $position, $ret)) {
        $ret = "";
        if ($position['j'] < $objPos['j']) {
            $ret .= str_repeat(">", $objPos['j'] - $position['j']);
        }
        if ($position['i'] > $objPos['i']) {
            $ret .= str_repeat("^", $position['i'] - $objPos['i']);
        }
        if ($position['i'] < $objPos['i']) {
            $ret .= str_repeat("v", $objPos['i'] - $position['i']);
        }
        if ($position['j'] > $objPos['j']) {
            $ret .= str_repeat("<", $position['j'] - $objPos['j']);
        }
    }

    $moveCache[$key] = $ret;
    return $ret;
}

function solve(string $code, int $robots, array $keyPad, array $robotPad, int $maxRobots): int {
    static $solveCache = [];
    $key = serialize([$code, $robots, $maxRobots]);
    if (isset($solveCache[$key])) {
        return $solveCache[$key];
    }

    if ($robots <= 0) {
        return strlen($code);
    }

    $ret = 0;
    $posi = 3;
    $posj = 2;
    if ($robots != $maxRobots) {
        $posi = 0;
    }

    $moves = "";
    for ($i = 0; $i < strlen($code); $i++) {
        $ch = $code[$i];
        if ($robots == $maxRobots) {
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
        $ret += solve($moves . "A", $robots - 1, $keyPad, $robotPad, $maxRobots);
    }

    $solveCache[$key] = $ret;
    return $ret;
}

$content = file_get_contents("input.txt");
$maxRobots = 26;
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
$codes = explode("\n", trim($content));

foreach ($codes as $code) {
    $code = trim($code);
    if ($code == "") {
        continue;
    }

    $numericPart = 0;
    for ($i = 0; $i < strlen($code); $i++) {
        if ($code[$i] >= '0' && $code[$i] <= '9') {
            $numericPart = $numericPart * 10 + intval($code[$i]);
        }
    }

    $sv = solve($code, $maxRobots, $keyPad, $robotPad, $maxRobots);
    $ret += $sv * $numericPart;
}

echo $ret . "\n";
?>
