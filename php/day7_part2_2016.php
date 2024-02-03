
<?php

$input = file_get_contents("input.txt");
$ips = explode("\n", $input);

$sslCount = 0;
foreach ($ips as $ip) {
    if (supportsSSL($ip)) {
        $sslCount++;
    }
}

echo $sslCount . PHP_EOL;

function supportsSSL($ip) {
    preg_match_all('/\[[a-z]+\]/', $ip, $bracketContents);
    $ip = preg_replace('/\[[a-z]+\]/', '-', $ip);

    foreach (findABAs($ip) as $aba) {
        $bab = $aba[1] . $aba[0] . $aba[1];
        foreach ($bracketContents[0] as $bracketContent) {
            if (strpos($bracketContent, $bab) !== false) {
                return true;
            }
        }
    }

    return false;
}

function findABAs($s) {
    $abas = [];
    for ($i = 0; $i < strlen($s) - 2; $i++) {
        if ($s[$i] != $s[$i+1] && $s[$i] == $s[$i+2]) {
            $abas[] = substr($s, $i, 3);
        }
    }
    return $abas;
}
?>
