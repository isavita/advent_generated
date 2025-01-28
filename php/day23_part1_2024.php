
<?php

function solve() {
    $connections = [];
    $computers = [];

    // Read input from file
    $file = fopen("input.txt", "r");
    if ($file) {
        while (($line = fgets($file)) !== false) {
            $line = trim($line);
            if (empty($line)) continue;

            list($comp1, $comp2) = explode('-', $line);
            $computers[$comp1] = true;
            $computers[$comp2] = true;

            $connections[$comp1][] = $comp2;
            $connections[$comp2][] = $comp1;
        }
        fclose($file);
    } else {
        die("Unable to open input.txt");
    }

    $computer_names = array_keys($computers);
    $triangle_count = 0;

    for ($i = 0; $i < count($computer_names); $i++) {
        for ($j = $i + 1; $j < count($computer_names); $j++) {
            for ($k = $j + 1; $k < count($computer_names); $k++) {
                $comp1 = $computer_names[$i];
                $comp2 = $computer_names[$j];
                $comp3 = $computer_names[$k];

                if (areConnected($connections, $comp1, $comp2) &&
                    areConnected($connections, $comp1, $comp3) &&
                    areConnected($connections, $comp2, $comp3)) {

                    if (startsWithT($comp1) || startsWithT($comp2) || startsWithT($comp3)) {
                        $triangle_count++;
                    }
                }
            }
        }
    }

    echo $triangle_count . "\n";
}

function areConnected($connections, $comp1, $comp2) {
    return isset($connections[$comp1]) && in_array($comp2, $connections[$comp1]);
}

function startsWithT($computerName) {
    return strtolower(substr($computerName, 0, 1)) === 't';
}

solve();

?>
