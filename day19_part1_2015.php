
<?php

$input = file_get_contents("input.txt");
$lines = explode("\n", $input);

$replacements = [];
$molecule = "";
foreach ($lines as $line) {
    if ($line == "") {
        continue;
    }
    if (strpos($line, " => ") !== false) {
        $replacements[] = $line;
    } else {
        $molecule = $line;
    }
}

$molecules = [];
foreach ($replacements as $replacement) {
    $parts = explode(" => ", $replacement);
    for ($i = 0; $i < strlen($molecule); $i++) {
        if (substr($molecule, $i, strlen($parts[0])) === $parts[0]) {
            $newMolecule = substr($molecule, 0, $i) . $parts[1] . substr($molecule, $i + strlen($parts[0]));
            $molecules[$newMolecule] = true;
        }
    }
}

echo count($molecules) . "\n";
?>
