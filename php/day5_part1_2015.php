
<?php

$input = trim(file_get_contents("input.txt"));

$nice = 0;
$disallowPattern = "/(ab|cd|pq|xy)/";
$lines = explode("\n", $input);

foreach ($lines as $line) {
    $vowels = 0;
    for ($i = 0; $i < strlen($line); $i++) {
        if (strpos("aeiou", $line[$i]) !== false) {
            $vowels++;
        }
    }
    $hasDouble = false;
    for ($i = 0; $i < strlen($line) - 1; $i++) {
        if ($line[$i] == $line[$i + 1]) {
            $hasDouble = true;
            break;
        }
    }
    if ($vowels >= 3 && !preg_match($disallowPattern, $line) && $hasDouble) {
        $nice++;
    }
}

echo $nice . "\n";
?>
