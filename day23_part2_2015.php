
<?php

$data = file_get_contents("input.txt");
$instructions = explode("\n", trim($data));

$registers = ["a" => 1, "b" => 0];

for ($i = 0; $i < count($instructions); $i++) {
    $parts = explode(" ", $instructions[$i]);

    switch ($parts[0]) {
        case "hlf":
            $registers[$parts[1]] /= 2;
            break;
        case "tpl":
            $registers[$parts[1]] *= 3;
            break;
        case "inc":
            $registers[$parts[1]]++;
            break;
        case "jmp":
            $i += (int)$parts[1] - 1;
            break;
        case "jie":
            if ($registers[$parts[1][0]] % 2 == 0) {
                $i += (int)$parts[2] - 1;
            }
            break;
        case "jio":
            if ($registers[$parts[1][0]] == 1) {
                $i += (int)$parts[2] - 1;
            }
            break;
        default:
            die("Unknown instruction: " . $parts[0]);
    }
}

echo $registers["b"] . "\n";
?>
