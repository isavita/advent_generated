<?php

const HASH_TABLE_SIZE = 256;

function hashString($str) {
    $res = 0;
    $length = strlen($str);
    for ($i = 0; $i < $length; $i++) {
        $char = ord($str[$i]);
        $res += $char;
        $res *= 17;
        $res %= HASH_TABLE_SIZE;
    }
    return $res;
}

function parseStep($stepStr) {
    preg_match("/([a-zA-Z]+)(=|-)(\d+)/", $stepStr, $matches);
    $step = [
        'Label' => $matches[1],
        'NumBox' => hashString($matches[1]),
        'Operation' => $matches[2],
        'Number' => intval($matches[3])
    ];
    return $step;
}

function solve($input) {
    $line = $input[0];
    $steps = explode(",", $line);
    $res = 0;
    foreach ($steps as $step) {
        $res += hashString($step);
    }
    return $res;
}

function readFileContent($fileName) {
    $fileContent = file_get_contents($fileName);
    return explode("\n", trim($fileContent));
}

$input = readFileContent("input.txt");
echo solve($input) . PHP_EOL;

?>