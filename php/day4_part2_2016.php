<?php
function isRealRoom($room) {
    [$encryptedName, $checksum] = explode('[', $room);
    $checksum = rtrim($checksum, ']');
    $parts = explode('-', $encryptedName);
    $sectorIDPart = array_pop($parts);
    $encryptedName = implode('-', $parts);

    $letterCounts = [];
    foreach (str_split($encryptedName) as $letter) {
        if ($letter !== '-') {
            if (!isset($letterCounts[$letter])) {
                $letterCounts[$letter] = 0;
            }
            $letterCounts[$letter]++;
        }
    }

    array_multisort(array_values($letterCounts), SORT_DESC, array_keys($letterCounts), SORT_ASC, $letterCounts);
    $calculatedChecksum = implode('', array_slice(array_keys($letterCounts), 0, 5));

    return $calculatedChecksum === $checksum;
}

function getSectorID($room) {
    preg_match('/(\d+)/', $room, $matches);
    return (int) $matches[0];
}

function decryptName($room) {
    $parts = explode('-', $room);
    $sectorIDPart = array_pop($parts);
    $sectorID = getSectorID($room);
    $decryptedName = '';

    foreach ($parts as $part) {
        foreach (str_split($part) as $letter) {
            $shiftedLetter = chr((ord($letter) - ord('a') + $sectorID) % 26 + ord('a'));
            $decryptedName .= $shiftedLetter;
        }
        $decryptedName .= ' ';
    }

    return trim($decryptedName);
}

$file = fopen("input.txt", "r");
if (!$file) {
    die("Failed to open input.txt");
}

while (($line = fgets($file)) !== false) {
    $line = trim($line);
    if (isRealRoom($line)) {
        $decryptedName = decryptName($line);
        if (strpos($decryptedName, 'northpole object') !== false) {
            echo getSectorID($line) . "\n";
            break;
        }
    }
}

fclose($file);
?>