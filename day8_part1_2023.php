
<?php

$input = trim(file_get_contents("input.txt"));
$lines = explode("\n", $input);

$re = "/[A-Z]{3}/";

$desertMap = [];

for ($i = 2; $i < count($lines); $i++) {
    $line = $lines[$i];
    if (empty($line)) {
        continue;
    }

    preg_match_all($re, $line, $matches);
    $desertMap[$matches[0][0]] = [
        "left" => $matches[0][1],
        "right" => $matches[0][2]
    ];
}

$current = "AAA";
$steps = 0;
$elemToMatch = "ZZZ";

while ($current != $elemToMatch) {
    foreach (str_split(trim($lines[0])) as $direction) {
        if ($direction == 'R') {
            $current = $desertMap[$current]["right"];
        } elseif ($direction == 'L') {
            $current = $desertMap[$current]["left"];
        }
        $steps++;

        if ($current == $elemToMatch) {
            break;
        }
    }
}

echo $steps . "\n";
?>
