
<?php

$file = fopen("input.txt", "r");
$sum = 0;

while (!feof($file)) {
    $line = trim(fgets($file));
    list($firstDigit, $lastDigit) = findFirstAndLastDigit($line);
    $sum += 10*$firstDigit + $lastDigit;
}

fclose($file);

echo $sum . PHP_EOL;

function findFirstAndLastDigit($line) {
    $digits = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"];

    $firstDigit = 0;
    $lastDigit = 0;

    for ($i = 0; $i < strlen($line); $i++) {
        $char = $line[$i];
        $digitStr = (string)$char;

        if ($digitStr >= "0" && $digitStr <= "9") {
            if ($firstDigit == 0) {
                $firstDigit = (int)$digitStr;
            }
            $lastDigit = (int)$digitStr;
        } else {
            foreach ($digits as $j => $digit) {
                if (strpos($line, $digit, $i) === $i) {
                    if ($firstDigit == 0) {
                        $firstDigit = $j;
                    }
                    $lastDigit = $j;
                    break;
                }
            }
        }
    }

    return [$firstDigit, $lastDigit];
}
?>
