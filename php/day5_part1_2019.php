
<?php

$input = file_get_contents("input.txt");
$program = array_map('intval', explode(",", trim($input)));

function getMode($instruction, $position) {
    return (int)($instruction / pow(10, $position + 1)) % 10;
}

function getParam($program, $pointer, $mode) {
    if ($mode == 0) {
        return $program[$program[$pointer]];
    }
    return $program[$pointer];
}

function runProgram($program, $input) {
    $output = 0;
    $pointer = 0;
    while ($pointer < count($program)) {
        $instruction = $program[$pointer];
        $opcode = $instruction % 100;

        switch ($opcode) {
            case 1:
            case 2:
                $param1 = getParam($program, $pointer + 1, getMode($instruction, 1));
                $param2 = getParam($program, $pointer + 2, getMode($instruction, 2));
                $result = 0;
                if ($opcode == 1) {
                    $result = $param1 + $param2;
                } else {
                    $result = $param1 * $param2;
                }
                $program[$program[$pointer + 3]] = $result;
                $pointer += 4;
                break;
            case 3:
                $program[$program[$pointer + 1]] = $input;
                $pointer += 2;
                break;
            case 4:
                $output = getParam($program, $pointer + 1, getMode($instruction, 1));
                $pointer += 2;
                break;
            case 99:
                return $output;
            default:
                throw new Exception("Unknown opcode: " . $opcode);
        }
    }
    return $output;
}

echo runProgram($program, 1) . PHP_EOL;

?>
