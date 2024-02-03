
<?php

$input = file_get_contents("input.txt");
echo firstNUnique($input, 14);

function firstNUnique($s, $n) {
    for ($i = $n; $i < strlen($s); $i++) {
        $b = str_split(substr($s, $i - $n, $n));
        if (count($b) == count(array_unique($b))) {
            return $i;
        }
    }
    return -1;
}
?>
