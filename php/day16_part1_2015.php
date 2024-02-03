
<?php

$mfcsam = [
    "children" => 3, "cats" => 7, "samoyeds" => 2, "pomeranians" => 3,
    "akitas" => 0, "vizslas" => 0, "goldfish" => 5, "trees" => 3,
    "cars" => 2, "perfumes" => 1,
];

$file = fopen("input.txt", "r");
if ($file === false) {
    echo "Error opening file\n";
    return;
}

while (($line = fgets($file)) !== false) {
    $parts = explode(" ", $line);
    $sueNumber = substr($parts[1], 0, -1);

    $matches = true;
    for ($i = 2; $i < count($parts); $i += 2) {
        $item = substr($parts[$i], 0, -1);
        $count = (int)substr($parts[$i + 1], 0, -1);
        if ($mfcsam[$item] !== $count) {
            $matches = false;
            break;
        }
    }

    if ($matches) {
        echo $sueNumber . "\n";
        break;
    }
}

fclose($file);
