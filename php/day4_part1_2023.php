
<?php

$totalPoints = 0;
$file = fopen("input.txt", "r");

if ($file) {
    while (($line = fgets($file)) !== false) {
        $parts = explode(":", $line);
        $numbersPart = trim($parts[1]);
        $numberSets = explode("|", $numbersPart);
        $winningNumbers = array_filter(explode(" ", trim($numberSets[0])));
        $myNumbers = array_filter(explode(" ", trim($numberSets[1])));

        $matches = array_intersect($winningNumbers, $myNumbers);
        $numMatches = count($matches);

        if ($numMatches > 0) {
            $cardPoints = 1;
            for ($i = 1; $i < $numMatches; $i++) {
                $cardPoints *= 2;
            }
            $totalPoints += $cardPoints;
        }
    }
    fclose($file);
}

echo $totalPoints . "\n";

?>
