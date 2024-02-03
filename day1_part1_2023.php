
<?php

$sum = 0;
$handle = fopen("input.txt", "r");

if ($handle) {
    while (($line = fgets($handle)) !== false) {
        if ($line == "\n") {
            continue;
        }
        
        $firstDigit = -1;
        $lastDigit = -1;
        
        for ($i = 0; $i < strlen($line); $i++) {
            $char = $line[$i];
            if (is_numeric($char)) {
                if ($firstDigit == -1) {
                    $firstDigit = intval($char);
                }
                $lastDigit = intval($char);
            }
        }
        
        if ($firstDigit != -1 && $lastDigit != -1) {
            $value = intval($firstDigit . $lastDigit);
            $sum += $value;
        }
    }

    fclose($handle);
} else {
    echo "Error opening file";
}

echo $sum;
?>
