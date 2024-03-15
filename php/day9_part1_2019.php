<?php
$file = fopen("input.txt", "r");
$program = explode(",", trim(fgets($file)));
fclose($file);

$memory = array_fill_keys(range(0, count($program) - 1), 0);
foreach ($program as $i => $value) {
    $memory[$i] = (int)$value;
}

echo runIntcode($memory) . PHP_EOL;

function runIntcode($memory) {
    $output = 0;
    $ip = 0;
    $relativeBase = 0;

    while (true) {
        $opcode = $memory[$ip] % 100;
        $modes = str_pad(floor($memory[$ip] / 100), 3, "0", STR_PAD_LEFT);

        $getParam = function ($offset) use (&$memory, &$ip, &$relativeBase, $modes) {
            $mode = (int)$modes[strlen($modes) - $offset];
            $param = $memory[$ip + $offset];
            switch ($mode) {
                case 0:
                    return $memory[$param];
                case 1:
                    return $param;
                case 2:
                    return $memory[$relativeBase + $param];
                default:
                    throw new Exception("Unknown parameter mode");
            }
        };

        $setParam = function ($offset, $value) use (&$memory, &$ip, &$relativeBase, $modes) {
            $mode = (int)$modes[strlen($modes) - $offset];
            $param = $memory[$ip + $offset];
            switch ($mode) {
                case 0:
                    $memory[$param] = $value;
                    break;
                case 2:
                    $memory[$relativeBase + $param] = $value;
                    break;
                default:
                    throw new Exception("Unknown parameter mode");
            }
        };

        switch ($opcode) {
            case 1:
                $setParam(3, $getParam(1) + $getParam(2));
                $ip += 4;
                break;
            case 2:
                $setParam(3, $getParam(1) * $getParam(2));
                $ip += 4;
                break;
            case 3:
                $setParam(1, 1); // Test mode input
                $ip += 2;
                break;
            case 4:
                $output = $getParam(1);
                $ip += 2;
                break;
            case 5:
                if ($getParam(1) != 0) {
                    $ip = $getParam(2);
                } else {
                    $ip += 3;
                }
                break;
            case 6:
                if ($getParam(1) == 0) {
                    $ip = $getParam(2);
                } else {
                    $ip += 3;
                }
                break;
            case 7:
                if ($getParam(1) < $getParam(2)) {
                    $setParam(3, 1);
                } else {
                    $setParam(3, 0);
                }
                $ip += 4;
                break;
            case 8:
                if ($getParam(1) == $getParam(2)) {
                    $setParam(3, 1);
                } else {
                    $setParam(3, 0);
                }
                $ip += 4;
                break;
            case 9:
                $relativeBase += $getParam(1);
                $ip += 2;
                break;
            case 99:
                return $output;
            default:
                throw new Exception("Unknown opcode: " . $opcode);
        }
    }
}
?>