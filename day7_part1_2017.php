
<?php

$data = file_get_contents("input.txt");
$lines = explode("\n", trim($data));

$holderMap = [];
$heldMap = [];

foreach ($lines as $line) {
    preg_match_all('/[a-z]+/', $line, $names);
    $holder = $names[0][0];
    $holderMap[$holder] = true;

    if (count($names[0]) > 1) {
        foreach (array_slice($names[0], 1) as $name) {
            $heldMap[$name] = true;
        }
    }
}

foreach (array_keys($holderMap) as $holder) {
    if (!isset($heldMap[$holder])) {
        echo $holder . "\n";
        break;
    }
}
?>
