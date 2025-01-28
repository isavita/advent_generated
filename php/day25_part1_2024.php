
<?php
$raw = [];
$file = fopen("input.txt", "r");
if ($file) {
    while (($line = fgets($file)) !== false) {
        $line = trim($line);
        if ($line !== "") {
            $raw[] = $line;
        }
    }
    fclose($file);
} else {
    die("0\n");
}

if (count($raw) % 7 !== 0) {
    echo "0\n";
    return;
}

$locks = [];
$keys = [];

function allChar(string $s, string $ch): bool {
    return strspn($s, $ch) === strlen($s);
}

function parseLock(array $b): array {
    $h = array_fill(0, 5, 0);
    for ($c = 0; $c < 5; $c++) {
        for ($r = 1; $r < 7; $r++) {
            if ($b[$r][$c] === '#') {
                $h[$c]++;
            } else {
                break;
            }
        }
    }
    return $h;
}

function parseKey(array $b): array {
    $h = array_fill(0, 5, 0);
    for ($c = 0; $c < 5; $c++) {
        for ($r = 5; $r >= 0; $r--) {
            if ($b[$r][$c] === '#') {
                $h[$c]++;
            } else {
                break;
            }
        }
    }
    return $h;
}

function fits(array $lock, array $key): bool {
    for ($i = 0; $i < 5; $i++) {
        if ($lock[$i] + $key[$i] > 5) {
            return false;
        }
    }
    return true;
}

for ($i = 0; $i + 7 <= count($raw); $i += 7) {
    $block = array_slice($raw, $i, 7);
    $valid = true;
    foreach ($block as $ln) {
        if (strlen($ln) < 5) {
            $valid = false;
            break;
        }
    }
    if (!$valid) {
        continue;
    }
    if (allChar($block[0], '#')) {
        $locks[] = parseLock($block);
    } else {
        $keys[] = parseKey($block);
    }
}

$count = 0;
foreach ($locks as $lock) {
    foreach ($keys as $key) {
        if (fits($lock, $key)) {
            $count++;
        }
    }
}

echo $count . "\n";
?>
