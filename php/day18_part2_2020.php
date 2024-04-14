<?php

function main() {
    $input = trim(file_get_contents("input.txt"));
    $ans = solve($input);
    echo $ans;
}

function solve($input) {
    $lines = parseInput($input);
    $total = 0;

    foreach ($lines as $line) {
        $total += doMaths($line, 'calcFlatSlicePart');
    }

    return $total;
}

function parseInput($input) {
    $ans = [];
    $lines = explode("\n", $input);
    foreach ($lines as $l) {
        $ans[] = str_split(str_replace(' ', '', $l));
    }
    return $ans;
}

function doMaths($input, $flatteningFunc) {
    $stackOpenIndices = [];
    $stackFlattened = [];
    for ($i = 0; $i < count($input); $i++) {
        $stackFlattened[] = $input[$i];
        if ($input[$i] === "(") {
            $stackOpenIndices[] = count($stackFlattened) - 1;
        } elseif ($input[$i] === ")") {
            $openIndex = array_pop($stackOpenIndices);
            $sliToFlatten = array_slice($stackFlattened, $openIndex + 1, count($stackFlattened) - $openIndex - 2);
            $stackFlattened[$openIndex] = $flatteningFunc($sliToFlatten);
            $stackFlattened = array_slice($stackFlattened, 0, $openIndex + 1);
        }
    }
    return intval($flatteningFunc($stackFlattened));
}

function calcFlatSlicePart($input) {
    for ($i = 1; $i < count($input) - 1; $i++) {
        if ($input[$i] === "+") {
            if (is_numeric($input[$i - 1]) && is_numeric($input[$i + 1])) {
                $input[$i - 1] = strval(intval($input[$i - 1]) + intval($input[$i + 1]));
                array_splice($input, $i, 2);
                $i--;
            }
        }
    }

    for ($i = 1; $i < count($input) - 1; $i++) {
        if ($input[$i] === "*") {
            if (is_numeric($input[$i - 1]) && is_numeric($input[$i + 1])) {
                $input[$i - 1] = strval(intval($input[$i - 1]) * intval($input[$i + 1]));
                array_splice($input, $i, 2);
                $i--;
            }
        }
    }

    return $input[0];
}

main();