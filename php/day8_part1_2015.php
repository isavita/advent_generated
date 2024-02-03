
<?php

$file = fopen("input.txt", "r");
if ($file === false) {
    echo "Error opening file\n";
    return;
}

$totalDiff = 0;
while (($line = fgets($file)) !== false) {
    $codeLength = strlen($line);
    $memoryLength = calculateMemoryLength($line);
    $totalDiff += $codeLength - $memoryLength;
}

fclose($file);

echo $totalDiff . "\n";

function calculateMemoryLength($s) {
    $length = 0;
    $inEscape = false;
    $hexCount = 0;

    for ($i = 1; $i < strlen($s) - 1; $i++) {
        if ($hexCount > 0) {
            $hexCount--;
        } elseif ($inEscape) {
            if ($s[$i] === 'x') {
                $hexCount = 2;
            }
            $inEscape = false;
            $length++;
        } elseif ($s[$i] === '\\') {
            $inEscape = true;
        } else {
            $length++;
        }
    }

    return $length;
}
?>
