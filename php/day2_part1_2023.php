
<?php

$file = fopen("input.txt", "r");
$totalSum = 0;

while (!feof($file)) {
    $line = trim(fgets($file));
    $matches = [];
    preg_match('/Game (\d+): (.+)/', $line, $matches);

    if (count($matches) == 3) {
        $gameId = intval($matches[1]);
        $rounds = explode(";", $matches[2]);
        $isValid = true;

        foreach ($rounds as $round) {
            $cubes = [];
            preg_match_all('/(\d+) (red|green|blue)/', $round, $cubes);
            $red = 0;
            $green = 0;
            $blue = 0;

            foreach ($cubes[0] as $cube) {
                $cubeMatches = [];
                preg_match('/(\d+) (red|green|blue)/', $cube, $cubeMatches);
                $count = intval($cubeMatches[1]);
                $color = $cubeMatches[2];

                switch ($color) {
                    case "red":
                        $red += $count;
                        break;
                    case "green":
                        $green += $count;
                        break;
                    case "blue":
                        $blue += $count;
                        break;
                }

                if ($red > 12 || $green > 13 || $blue > 14) {
                    $isValid = false;
                    break;
                }
            }

            if (!$isValid) {
                break;
            }
        }

        if ($isValid) {
            $totalSum += $gameId;
        }
    }
}

fclose($file);

echo $totalSum . PHP_EOL;
?>
