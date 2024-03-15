<?php
$file = fopen("input.txt", "r");
$programStr = fgets($file);
fclose($file);

$program = array_map('intval', explode(',', $programStr));

$input = 5;
$output = 0;
$i = 0;
while (true) {
    $opcode = $program[$i] % 100;
    $modes = (int)($program[$i] / 100);
    $param1Mode = $modes % 10;
    $modes = (int)($modes / 10);
    $param2Mode = $modes % 10;

    switch ($opcode) {
        case 1:
            $p1 = getValue($program, $i + 1, $param1Mode);
            $p2 = getValue($program, $i + 2, $param2Mode);
            $p3 = $program[$i + 3];
            $program[$p3] = $p1 + $p2;
            $i += 4;
            break;
        case 2:
            $p1 = getValue($program, $i + 1, $param1Mode);
            $p2 = getValue($program, $i + 2, $param2Mode);
            $p3 = $program[$i + 3];
            $program[$p3] = $p1 * $p2;
            $i += 4;
            break;
        case 3:
            $program[$program[$i + 1]] = $input;
            $i += 2;
            break;
        case 4:
            $output = getValue($program, $i + 1, $param1Mode);
            echo $output . "\n";
            $i += 2;
            break;
        case 5:
            $p1 = getValue($program, $i + 1, $param1Mode);
            $p2 = getValue($program, $i + 2, $param2Mode);
            if ($p1 != 0) {
                $i = $p2;
            } else {
                $i += 3;
            }
            break;
        case 6:
            $p1 = getValue($program, $i + 1, $param1Mode);
            $p2 = getValue($program, $i + 2, $param2Mode);
            if ($p1 == 0) {
                $i = $p2;
            } else {
                $i += 3;
            }
            break;
        case 7:
            $p1 = getValue($program, $i + 1, $param1Mode);
            $p2 = getValue($program, $i + 2, $param2Mode);
            $p3 = $program[$i + 3];
            if ($p1 < $p2) {
                $program[$p3] = 1;
            } else {
                $program[$p3] = 0;
            }
            $i += 4;
            break;
        case 8:
            $p1 = getValue($program, $i + 1, $param1Mode);
            $p2 = getValue($program, $i + 2, $param2Mode);
            $p3 = $program[$i + 3];
            if ($p1 == $p2) {
                $program[$p3] = 1;
            } else {
                $program[$p3] = 0;
            }
            $i += 4;
            break;
        case 99:
            return;
        default:
            die("Invalid opcode");
    }
}

function getValue($program, $pos, $mode)
{
    if ($mode == 0) {
        return $program[$program[$pos]];
    } else {
        return $program[$pos];
    }
}