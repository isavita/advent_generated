
<?php

$input = file_get_contents("input.txt");
$program = explode(",", trim($input));
echo countBlocks($program);

function decode($n) {
    $op = $n % 100;
    $n = intdiv($n, 100);
    $modes = [];
    for ($i = 0; $i < 3; $i++) {
        $modes[] = $n % 10;
        $n = intdiv($n, 10);
    }
    return [$op, $modes];
}

function get($ip, $mode, $data, $relbase) {
    switch ($mode) {
        case 0:
            return $data[$data[$ip]];
        case 1:
            return $data[$ip];
        case 2:
            return $data[$relbase + $data[$ip]];
    }
}

function set($ip, $mode, $val, &$data, $relbase) {
    switch ($mode) {
        case 0:
            $data[$data[$ip]] = $val;
            break;
        case 2:
            $data[$relbase + $data[$ip]] = $val;
            break;
    }
}

function run($program) {
    $data = [];
    foreach ($program as $i => $n) {
        $data[$i] = $n;
    }

    $ip = 0;
    $relbase = 0;
    $out = [];
    $in = [];

    while (true) {
        list($op, $modes) = decode($data[$ip]);
        switch ($op) {
            case 1:
                $val = get($ip+1, $modes[0], $data, $relbase) + get($ip+2, $modes[1], $data, $relbase);
                set($ip+3, $modes[2], $val, $data, $relbase);
                $ip += 4;
                break;
            case 2:
                $val = get($ip+1, $modes[0], $data, $relbase) * get($ip+2, $modes[1], $data, $relbase);
                set($ip+3, $modes[2], $val, $data, $relbase);
                $ip += 4;
                break;
            case 3:
                if (empty($in)) {
                    break 2;
                }
                set($ip+1, $modes[0], array_shift($in), $data, $relbase);
                $ip += 2;
                break;
            case 4:
                $out[] = get($ip+1, $modes[0], $data, $relbase);
                $ip += 2;
                break;
            case 5:
                if (get($ip+1, $modes[0], $data, $relbase) != 0) {
                    $ip = get($ip+2, $modes[1], $data, $relbase);
                } else {
                    $ip += 3;
                }
                break;
            case 6:
                if (get($ip+1, $modes[0], $data, $relbase) == 0) {
                    $ip = get($ip+2, $modes[1], $data, $relbase);
                } else {
                    $ip += 3;
                }
                break;
            case 7:
                if (get($ip+1, $modes[0], $data, $relbase) < get($ip+2, $modes[1], $data, $relbase)) {
                    set($ip+3, $modes[2], 1, $data, $relbase);
                } else {
                    set($ip+3, $modes[2], 0, $data, $relbase);
                }
                $ip += 4;
                break;
            case 8:
                if (get($ip+1, $modes[0], $data, $relbase) == get($ip+2, $modes[1], $data, $relbase)) {
                    set($ip+3, $modes[2], 1, $data, $relbase);
                } else {
                    set($ip+3, $modes[2], 0, $data, $relbase);
                }
                $ip += 4;
                break;
            case 9:
                $relbase += get($ip+1, $modes[0], $data, $relbase);
                $ip += 2;
                break;
            case 99:
                return $out;
            default:
                throw new Exception("Unknown opcode: {$op}");
        }
    }
}

function countBlocks($program) {
    $out = run($program);
    $count = 0;
    for ($i = 0; $i < count($out); $i += 3) {
        if ($out[$i+2] === 2) {
            $count++;
        }
    }
    return $count;
}
?>
