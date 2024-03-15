<?php
// Read input from file
$file = fopen("input.txt", "r");
$program = explode(",", trim(fgets($file)));
fclose($file);

// Convert input to integers and extend memory
$memory = array_fill_keys(range(0, count($program) - 1), 0);
foreach ($program as $i => $value) {
    $memory[$i] = (int)$value;
}

// Run the Intcode program in sensor boost mode
echo runIntcode($memory, 2);

function runIntcode($memory, $input)
{
    $output = 0;
    $ip = 0;
    $relativeBase = 0;

    while (true) {
        $opcode = $memory[$ip] % 100;
        $modes = str_pad(floor($memory[$ip] / 100), 3, "0", STR_PAD_LEFT);

        $getParam = function ($offset) use (&$memory, $ip, $modes, $relativeBase) {
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

        $setParam = function ($offset, $value) use (&$memory, $ip, $modes, $relativeBase) {
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
                $setParam(1, $input);
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
                $setParam(3, $getParam(1) < $getParam(2) ? 1 : 0);
                $ip += 4;
                break;
            case 8:
                $setParam(3, $getParam(1) == $getParam(2) ? 1 : 0);
                $ip += 4;
                break;
            case 9:
                $relativeBase += $getParam(1);
                $ip += 2;
                break;
            case 99:
                return $output;
            default:
                throw new Exception("Unknown opcode: $opcode");
        }
    }
}
?>