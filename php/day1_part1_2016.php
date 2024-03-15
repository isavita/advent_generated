<?php
$file = fopen("input.txt", "r");
$instructions = explode(", ", trim(fread($file, filesize("input.txt"))));
fclose($file);

$pos = ["x" => 0, "y" => 0, "dirIndex" => 0];
$directions = [[0, 1], [1, 0], [0, -1], [-1, 0]]; // North, East, South, West

foreach ($instructions as $instruction) {
    $turn = substr($instruction, 0, 1);
    $blocks = (int) substr($instruction, 1);

    if ($turn == "R") {
        $pos["dirIndex"] = ($pos["dirIndex"] + 1) % 4;
    } else {
        $pos["dirIndex"] = ($pos["dirIndex"] - 1 + 4) % 4;
    }

    $pos["x"] += $directions[$pos["dirIndex"]][0] * $blocks;
    $pos["y"] += $directions[$pos["dirIndex"]][1] * $blocks;
}

echo abs($pos["x"]) + abs($pos["y"]);
?>