
<?php

const UP = [0, -1];
const DOWN = [0, 1];
const LEFT = [-1, 0];
const RIGHT = [1, 0];

function solve(string $input): float
{
    [$m, $steps] = parseInput($input);
    $robot = null;
    foreach ($m as $k => $v) {
        if ($v === '@') {
            $robot = parseComplex($k);
            break;
        }
    }
    foreach ($steps as $dir) {
        if (tryToStep($m, $robot, $dir)) {
            $robot = complexAdd($robot, $dir);
        }
    }
    $sum = 0.0;
    foreach ($m as $k => $v) {
        if ($v === '[' || $v === 'O') {
            $complexKey = parseComplex($k);
            $sum += $complexKey[0] + 100 * $complexKey[1];
        }
    }
    return $sum;
}

function tryToStep(array &$m, array $pos, array $dir): bool
{
    $orig = $m;
    $posKey = complexToString($pos);
    if (!isset($m[$posKey])) return false;
    if ($m[$posKey] === '.') {
        return true;
    } else if ($m[$posKey] === 'O' || $m[$posKey] === '@') {
        $nextPos = complexAdd($pos, $dir);
        if (tryToStep($m, $nextPos, $dir)) {
            $m[complexToString($nextPos)] = $m[$posKey];
            $m[$posKey] = '.';
            return true;
        }
    } else if ($m[$posKey] === ']') {
        if (tryToStep($m, complexAdd($pos, LEFT), $dir)) {
            return true;
        }
    } else if ($m[$posKey] === '[') {
        if ($dir === LEFT) {
            $nextPosLeft = complexAdd($pos, LEFT);
            if (tryToStep($m, $nextPosLeft, $dir)) {
                $m[complexToString($nextPosLeft)] = '[';
                $m[$posKey] = ']';
                $m[complexToString(complexAdd($pos, RIGHT))] = '.';
                return true;
            }
        } else if ($dir === RIGHT) {
            $nextPosRight2 = complexAdd($pos, complexAdd($dir, $dir));
            if (tryToStep($m, $nextPosRight2, $dir)) {
                $m[$posKey] = '.';
                $m[complexToString(complexAdd($pos, RIGHT))] = '[';
                $m[complexToString($nextPosRight2)] = ']';
                return true;
            }
        } else {
            $nextPosDir = complexAdd($pos, $dir);
            $nextPosRightDir = complexAdd($pos, complexAdd($dir, RIGHT));
            if (tryToStep($m, $nextPosDir, $dir) && tryToStep($m, $nextPosRightDir, $dir)) {
                $m[$posKey] = '.';
                $m[complexToString(complexAdd($pos, RIGHT))] = '.';
                $m[complexToString($nextPosDir)] = '[';
                $m[complexToString($nextPosRightDir)] = ']';
                return true;
            }
        }
    }
    $m = $orig;
    return false;
}

function scaleUp(string $input): string
{
    $s = $input;
    $s = str_replace("#", "##", $s);
    $s = str_replace(".", "..", $s);
    $s = str_replace("O", "[]", $s);
    $s = str_replace("@", "@.", $s);
    return $s;
}

function parseInput(string $input): array
{
    $blocks = explode("\n\n", trim($input));
    $lines = explode("\n", $blocks[0]);
    $m = [];
    foreach ($lines as $y => $line) {
        foreach (str_split($line) as $x => $char) {
            $m[complexToString([$x, $y])] = $char;
        }
    }
    $steps = [];
    $stepChars = str_replace("\n", "", $blocks[1]);
    foreach (str_split($stepChars) as $ch) {
        switch ($ch) {
            case '^':
                $steps[] = UP;
                break;
            case '<':
                $steps[] = LEFT;
                break;
            case '>':
                $steps[] = RIGHT;
                break;
            case 'v':
                $steps[] = DOWN;
                break;
        }
    }
    return [$m, $steps];
}

function complexToString(array $c): string {
    return $c[0] . "," . $c[1];
}

function parseComplex(string $s): array {
    return array_map('intval', explode(",", $s));
}

function complexAdd(array $c1, array $c2): array {
    return [$c1[0] + $c2[0], $c1[1] + $c2[1]];
}

$input = file_get_contents("input.txt");
echo intval(solve($input)) . "\n";
echo intval(solve(scaleUp($input))) . "\n";
?>
