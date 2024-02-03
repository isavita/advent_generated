
<?php

$claims = readClaims("input.txt");

$fabric = array_fill(0, 1000, array_fill(0, 1000, 0));

foreach ($claims as $claim) {
    for ($y = $claim['Y']; $y < $claim['Y'] + $claim['Height']; $y++) {
        for ($x = $claim['X']; $x < $claim['X'] + $claim['Width']; $x++) {
            $fabric[$y][$x]++;
        }
    }
}

foreach ($claims as $claim) {
    $overlap = false;
    for ($y = $claim['Y']; $y < $claim['Y'] + $claim['Height']; $y++) {
        for ($x = $claim['X']; $x < $claim['X'] + $claim['Width']; $x++) {
            if ($fabric[$y][$x] > 1) {
                $overlap = true;
                break;
            }
        }
        if ($overlap) {
            break;
        }
    }
    if (!$overlap) {
        echo $claim['ID'] . "\n";
        return;
    }
}

function readClaims($filename) {
    $file = fopen($filename, "r");
    $claims = [];

    while (!feof($file)) {
        $line = trim(fgets($file));
        $parts = explode(" ", $line);
        $id = intval(substr($parts[0], 1));
        $coords = explode(",", substr($parts[2], 0, -1));
        $x = intval($coords[0]);
        $y = intval($coords[1]);
        $dims = explode("x", $parts[3]);
        $width = intval($dims[0]);
        $height = intval($dims[1]);
        $claims[] = ['ID' => $id, 'X' => $x, 'Y' => $y, 'Width' => $width, 'Height' => $height];
    }

    fclose($file);
    return $claims;
}
?>
