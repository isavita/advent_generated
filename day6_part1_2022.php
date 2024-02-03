
<?php

function firstNUnique($s, $n) {
    for ($i = $n; $i < strlen($s); $i++) {
        $b = str_split(substr($s, $i - $n, $n));
        if (count($b) == count(array_unique($b))) {
            return $i;
        }
    }
    return -1;
}

$input = file_get_contents("input.txt");
echo firstNUnique(trim($input), 4) . PHP_EOL;

?>
