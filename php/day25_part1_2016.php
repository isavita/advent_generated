<?php
$file = fopen("input.txt", "r") or die("Unable to open file!");

$instructions = [];
while(!feof($file)) {
    $instructions[] = trim(fgets($file));
}
fclose($file);

for ($a = 1; ; $a++) {
    if (producesClockSignal($a, $instructions)) {
        echo $a . "\n";
        break;
    }
}

function producesClockSignal($a, $instructions) {
    $registers = ["a" => $a, "b" => 0, "c" => 0, "d" => 0];
    $lastOutput = 0;
    $outputCount = 0;

    for ($i = 0; $i < count($instructions); $i++) {
        $parts = explode(" ", $instructions[$i]);
        switch ($parts[0]) {
            case "cpy":
                $val = getValue($parts[1], $registers);
                $registers[$parts[2]] = $val;
                break;
            case "inc":
                $registers[$parts[1]]++;
                break;
            case "dec":
                $registers[$parts[1]]--;
                break;
            case "jnz":
                $val = getValue($parts[1], $registers);
                if ($val != 0) {
                    $jump = intval($parts[2]);
                    $i += $jump - 1;
                }
                break;
            case "out":
                $val = getValue($parts[1], $registers);
                if ($val != 0 && $val != 1) {
                    return false;
                }
                if ($outputCount > 0 && $val == $lastOutput) {
                    return false;
                }
                $lastOutput = $val;
                $outputCount++;
                if ($outputCount > 50) {
                    return true;
                }
                break;
        }
    }
    return false;
}

function getValue($s, $registers) {
    if (is_numeric($s)) {
        return intval($s);
    } else {
        return $registers[$s];
    }
}
?>