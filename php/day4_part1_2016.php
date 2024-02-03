
<?php

$file = fopen("input.txt", "r");
$sumOfSectorIDs = 0;

while (!feof($file)) {
    $line = trim(fgets($file));
    if (isRealRoom($line)) {
        $sumOfSectorIDs += getSectorID($line);
    }
}

echo $sumOfSectorIDs . PHP_EOL;

function isRealRoom($room) {
    $parts = explode("[", $room);
    $checksum = rtrim($parts[1], "]");
    $encryptedName = explode("-", $parts[0]);
    array_pop($encryptedName);

    $letterCounts = array_count_values(str_split(implode("", $encryptedName)));

    $counts = [];
    foreach ($letterCounts as $letter => $count) {
        $counts[] = (object) ['letter' => $letter, 'count' => $count];
    }

    usort($counts, function($a, $b) {
        if ($a->count == $b->count) {
            return $a->letter < $b->letter ? -1 : 1;
        }
        return $a->count > $b->count ? -1 : 1;
    });

    for ($i = 0; $i < strlen($checksum); $i++) {
        if ($checksum[$i] != $counts[$i]->letter) {
            return false;
        }
    }

    return true;
}

function getSectorID($room) {
    $parts = explode("-", $room);
    $sectorIDPart = $parts[count($parts) - 1];
    $sectorID = (int) explode("[", $sectorIDPart)[0];
    return $sectorID;
}

fclose($file);
?>
