
<?php

function runIntcode(array $program, int $input = 0): array
{
    $memory = $program;
    $pc = 0;
    $relativeBase = 0;
    $output = [];
    $inputIndex = 0;

    while (true) {
        $opcode = $memory[$pc] % 100;
        $modes = (int) ($memory[$pc] / 100);

        $getParam = function (int $offset) use (&$memory, $pc, $modes, $relativeBase): int {
            $mode = (int) ($modes / (10 ** ($offset - 1))) % 10;
            $address = $pc + $offset;
            switch ($mode) {
                case 0:
                    return $memory[$address] ?? 0;
                case 1:
                    return $address;
                case 2:
                    return ($memory[$address] ?? 0) + $relativeBase;
                default:
                    throw new Exception("Invalid mode: $mode");
            }
        };

        $getValue = function (int $offset) use (&$memory, $getParam): int {
            $address = $getParam($offset);
            return $memory[$address] ?? 0;
        };

        $setValue = function (int $offset, int $value) use (&$memory, $getParam): void {
            $address = $getParam($offset);
            $memory[$address] = $value;
        };

        switch ($opcode) {
            case 1: // Add
                $setValue(3, $getValue(1) + $getValue(2));
                $pc += 4;
                break;
            case 2: // Multiply
                $setValue(3, $getValue(1) * $getValue(2));
                $pc += 4;
                break;
            case 3: // Input
                $setValue(1, $input);
                $pc += 2;
                break;
            case 4: // Output
                $output[] = $getValue(1);
                $pc += 2;
                break;
            case 5: // Jump-if-true
                if ($getValue(1) != 0) {
                    $pc = $getValue(2);
                } else {
                    $pc += 3;
                }
                break;
            case 6: // Jump-if-false
                if ($getValue(1) == 0) {
                    $pc = $getValue(2);
                } else {
                    $pc += 3;
                }
                break;
            case 7: // Less than
                $setValue(3, $getValue(1) < $getValue(2) ? 1 : 0);
                $pc += 4;
                break;
            case 8: // Equals
                $setValue(3, $getValue(1) == $getValue(2) ? 1 : 0);
                $pc += 4;
                break;
            case 9: // Adjust relative base
                $relativeBase += $getValue(1);
                $pc += 2;
                break;
            case 99: // Halt
                return $output;
            default:
                throw new Exception("Invalid opcode: $opcode");
        }
    }
}

$program = array_map('intval', explode(',', trim(file_get_contents('input.txt'))));

// Part 1
$output = runIntcode($program);
$blocks = 0;
for ($i = 2; $i < count($output); $i += 3) {
    if ($output[$i] == 2) {
        $blocks++;
    }
}
echo "Part 1: $blocks\n";

// Part 2
$program[0] = 2;
$score = 0;
$ballX = 0;
$paddleX = 0;
$output = [];
$memory = $program;
$pc = 0;
$relativeBase = 0;
$inputIndex = 0;

while (true) {
    $opcode = $memory[$pc] % 100;
    $modes = (int) ($memory[$pc] / 100);

    $getParam = function (int $offset) use (&$memory, $pc, $modes, $relativeBase): int {
        $mode = (int) ($modes / (10 ** ($offset - 1))) % 10;
        $address = $pc + $offset;
        switch ($mode) {
            case 0:
                return $memory[$address] ?? 0;
            case 1:
                return $address;
            case 2:
                return ($memory[$address] ?? 0) + $relativeBase;
            default:
                throw new Exception("Invalid mode: $mode");
        }
    };

    $getValue = function (int $offset) use (&$memory, $getParam): int {
        $address = $getParam($offset);
        return $memory[$address] ?? 0;
    };

    $setValue = function (int $offset, int $value) use (&$memory, $getParam): void {
        $address = $getParam($offset);
        $memory[$address] = $value;
    };

    switch ($opcode) {
        case 1: // Add
            $setValue(3, $getValue(1) + $getValue(2));
            $pc += 4;
            break;
        case 2: // Multiply
            $setValue(3, $getValue(1) * $getValue(2));
            $pc += 4;
            break;
        case 3: // Input
            $input = ($ballX <=> $paddleX);
            $setValue(1, $input);
            $pc += 2;
            break;
        case 4: // Output
            $output[] = $getValue(1);
            $pc += 2;
            break;
        case 5: // Jump-if-true
            if ($getValue(1) != 0) {
                $pc = $getValue(2);
            } else {
                $pc += 3;
            }
            break;
        case 6: // Jump-if-false
            if ($getValue(1) == 0) {
                $pc = $getValue(2);
            } else {
                $pc += 3;
            }
            break;
        case 7: // Less than
            $setValue(3, $getValue(1) < $getValue(2) ? 1 : 0);
            $pc += 4;
            break;
        case 8: // Equals
            $setValue(3, $getValue(1) == $getValue(2) ? 1 : 0);
            $pc += 4;
            break;
        case 9: // Adjust relative base
            $relativeBase += $getValue(1);
            $pc += 2;
            break;
        case 99: // Halt
            break 2;
        default:
            throw new Exception("Invalid opcode: $opcode");
    }

    while (count($output) >= 3) {
        $x = array_shift($output);
        $y = array_shift($output);
        $tile = array_shift($output);

        if ($x == -1 && $y == 0) {
            $score = $tile;
        } else if ($tile == 3) {
            $paddleX = $x;
        } else if ($tile == 4) {
            $ballX = $x;
        }
    }
}

echo "Part 2: $score\n";
