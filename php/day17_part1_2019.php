
<?php

function runIntcode(array $program, array $input = []): array
{
    $output = [];
    $pc = 0;
    $relativeBase = 0;

    while (true) {
        $opcode = $program[$pc] % 100;
        $modes = (int) ($program[$pc] / 100);

        $getParam = function (int $offset) use ($program, $pc, $modes, $relativeBase): int {
            $mode = (int) ($modes / (10 ** ($offset - 1))) % 10;
            $address = $pc + $offset;
            switch ($mode) {
                case 0:
                    return $program[$program[$address] ?? 0] ?? 0;
                case 1:
                    return $program[$address] ?? 0;
                case 2:
                    return $program[$relativeBase + ($program[$address] ?? 0)] ?? 0;
                default:
                    throw new Exception("Invalid parameter mode: $mode");
            }
        };

        $setParam = function (int $offset, int $value) use (&$program, $pc, $modes, $relativeBase): void {
            $mode = (int) ($modes / (10 ** ($offset - 1))) % 10;
            $address = $pc + $offset;
            switch ($mode) {
                case 0:
                    $program[$program[$address] ?? 0] = $value;
                    break;
                case 2:
                    $program[$relativeBase + ($program[$address] ?? 0)] = $value;
                    break;
                default:
                    throw new Exception("Invalid parameter mode for set: $mode");
            }
        };

        switch ($opcode) {
            case 1: // Add
                $setParam(3, $getParam(1) + $getParam(2));
                $pc += 4;
                break;
            case 2: // Multiply
                $setParam(3, $getParam(1) * $getParam(2));
                $pc += 4;
                break;
            case 3: // Input
                if (empty($input)) {
                    throw new Exception("Input required but none provided");
                }
                $setParam(1, array_shift($input));
                $pc += 2;
                break;
            case 4: // Output
                $output[] = $getParam(1);
                $pc += 2;
                break;
            case 5: // Jump-if-true
                if ($getParam(1) != 0) {
                    $pc = $getParam(2);
                } else {
                    $pc += 3;
                }
                break;
            case 6: // Jump-if-false
                if ($getParam(1) == 0) {
                    $pc = $getParam(2);
                } else {
                    $pc += 3;
                }
                break;
            case 7: // Less than
                $setParam(3, $getParam(1) < $getParam(2) ? 1 : 0);
                $pc += 4;
                break;
            case 8: // Equals
                $setParam(3, $getParam(1) == $getParam(2) ? 1 : 0);
                $pc += 4;
                break;
            case 9: // Adjust relative base
                $relativeBase += $getParam(1);
                $pc += 2;
                break;
            case 99: // Halt
                return $output;
            default:
                throw new Exception("Invalid opcode: $opcode");
        }
    }
}

$input = file_get_contents('input.txt');
$program = array_map('intval', explode(',', trim($input)));

$output = runIntcode($program);
$outputString = implode('', array_map('chr', $output));

$lines = explode("\n", trim($outputString));
$grid = [];
foreach ($lines as $line) {
    $grid[] = str_split($line);
}

$height = count($grid);
$width = count($grid[0]);
$sum = 0;

for ($y = 1; $y < $height - 1; $y++) {
    for ($x = 1; $x < $width - 1; $x++) {
        if ($grid[$y][$x] == '#' &&
            $grid[$y - 1][$x] == '#' &&
            $grid[$y + 1][$x] == '#' &&
            $grid[$y][$x - 1] == '#' &&
            $grid[$y][$x + 1] == '#') {
            $sum += $x * $y;
        }
    }
}

echo $sum . "\n";
