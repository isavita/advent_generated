
<?php

$data = file_get_contents("input.txt");
$passphrases = explode("\n", trim($data));
$validCount = 0;

foreach ($passphrases as $passphrase) {
    $words = explode(" ", $passphrase);
    $wordSet = [];

    $valid = true;
    foreach ($words as $word) {
        if (in_array($word, $wordSet)) {
            $valid = false;
            break;
        }
        $wordSet[] = $word;
    }

    if ($valid) {
        $validCount++;
    }
}

echo $validCount . PHP_EOL;
?>
