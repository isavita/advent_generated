
<?php

$file = fopen("input.txt", "r");
$totalDiff = 0;

while (!feof($file)) {
    $line = trim(fgets($file));
    $originalLength = strlen($line);
    $encodedLength = calculateEncodedLength($line);
    $totalDiff += $encodedLength - $originalLength;
}

fclose($file);

echo $totalDiff;

function calculateEncodedLength($s) {
    $encoded = '"';
    for ($i = 0; $i < strlen($s); $i++) {
        $ch = $s[$i];
        if ($ch == '\\' || $ch == '"') {
            $encoded .= "\\";
        }
        $encoded .= $ch;
    }
    $encoded .= '"';
    return strlen($encoded);
}
?>
