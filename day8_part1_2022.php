
<?php

$grid = [];
$visible = [];

$handle = fopen("input.txt", "r");
if ($handle) {
    $y = 0;
    while (($line = fgets($handle)) !== false) {
        $line = str_split(trim($line));
        foreach ($line as $x => $b) {
            $grid["$x,$y"] = ord($b) - ord('0');
        }
        $y++;
    }

    foreach ($grid as $p => $value) {
        $p = explode(",", $p);
        $x = $p[0];
        $y = $p[1];
        foreach ([[0, 1], [0, -1], [1, 0], [-1, 0]] as $n) {
            $next = [$x, $y];
            while (true) {
                $next[0] += $n[0];
                $next[1] += $n[1];
                $key = implode(",", $next);
                if (array_key_exists($key, $grid)) {
                    if ($grid[$key] >= $grid["$x,$y"]) {
                        break;
                    }
                } else {
                    $visible["$x,$y"] = true;
                    break;
                }
            }
        }
    }

    echo count($visible) . PHP_EOL;

    fclose($handle);
} else {
    echo "Error opening the file." . PHP_EOL;
}
?>
