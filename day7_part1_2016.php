
<?php

$file = fopen("input.txt", "r");
$tlsCount = 0;

while (!feof($file)) {
    $line = trim(fgets($file));
    if (supportsTLS($line)) {
        $tlsCount++;
    }
}

fclose($file);
echo $tlsCount . PHP_EOL;

function supportsTLS($ip) {
    preg_match_all('/\[[a-z]+\]/', $ip, $bracketContents);
    foreach ($bracketContents[0] as $bracketContent) {
        if (containsABBA($bracketContent)) {
            return false;
        }
    }

    $ip = preg_replace('/\[[a-z]+\]/', '-', $ip);
    return containsABBA($ip);
}

function containsABBA($s) {
    for ($i = 0; $i < strlen($s) - 3; $i++) {
        if ($s[$i] != $s[$i+1] && $s[$i] == $s[$i+3] && $s[$i+1] == $s[$i+2]) {
            return true;
        }
    }
    return false;
}
?>
