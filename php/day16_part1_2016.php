<?php

const DISK_LENGTH = 272;

function readInitialState($filename) {
    $contents = file_get_contents($filename);
    if ($contents === false) {
        throw new Exception("Failed to read initial state");
    }
    return trim($contents);
}

function generateData($initialState, $length) {
    $data = $initialState;
    while (strlen($data) < $length) {
        $b = '';
        for ($i = strlen($data) - 1; $i >= 0; $i--) {
            $b .= $data[$i] == '0' ? '1' : '0';
        }
        $data = $data . '0' . $b;
    }
    return substr($data, 0, $length);
}

function calculateChecksum($data) {
    while (strlen($data) % 2 == 0) {
        $b = '';
        for ($i = 0; $i < strlen($data); $i += 2) {
            $b .= $data[$i] == $data[$i + 1] ? '1' : '0';
        }
        $data = $b;
    }
    return $data;
}

$initialState = readInitialState("input.txt");
$data = generateData($initialState, DISK_LENGTH);
$checksum = calculateChecksum($data);
echo "Checksum: " . $checksum . "\n";

?>