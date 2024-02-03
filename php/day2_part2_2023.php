
<?php

$file = fopen("input.txt", "r");
$regex = '/Game (\d+): (.+)/';
$cubeRegex = '/(\d+) (red|green|blue)/';
$totalPower = 0;

while (!feof($file)) {
    $line = fgets($file);
    if (preg_match($regex, $line, $matches)) {
        $rounds = explode(";", $matches[2]);
        $maxRed = $maxGreen = $maxBlue = 0;

        foreach ($rounds as $round) {
            preg_match_all($cubeRegex, $round, $cubes);
            $red = $green = $blue = 0;

            foreach ($cubes[0] as $cube) {
                preg_match($cubeRegex, $cube, $cubeMatches);
                $count = intval($cubeMatches[1]);
                switch ($cubeMatches[2]) {
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
            }

            $maxRed = max($maxRed, $red);
            $maxGreen = max($maxGreen, $green);
            $maxBlue = max($maxBlue, $blue);
        }

        $power = $maxRed * $maxGreen * $maxBlue;
        $totalPower += $power;
    }
}

fclose($file);
echo $totalPower . PHP_EOL;
?>
