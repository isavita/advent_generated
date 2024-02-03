
<?php

$lines = file("input.txt", FILE_IGNORE_NEW_LINES);

for ($i = 0; $i < count($lines) - 1; $i++) {
    for ($j = $i + 1; $j < count($lines); $j++) {
        $diff = 0;
        for ($k = 0; $k < strlen($lines[$i]); $k++) {
            if ($lines[$i][$k] != $lines[$j][$k]) {
                $diff++;
                if ($diff > 1) {
                    break;
                }
            }
        }
        if ($diff == 1) {
            $common = "";
            for ($k = 0; $k < strlen($lines[$i]); $k++) {
                if ($lines[$i][$k] == $lines[$j][$k]) {
                    $common .= $lines[$i][$k];
                }
            }
            echo $common . "\n";
            exit;
        }
    }
}
?>
