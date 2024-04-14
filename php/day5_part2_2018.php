<?php

function react($polymer) {
    $reactionOccurred = true;
    while ($reactionOccurred) {
        $reactionOccurred = false;
        $length = strlen($polymer);
        for ($i = 0; $i < $length - 1; $i++) {
            if ($polymer[$i] != $polymer[$i + 1] && strtoupper($polymer[$i]) == strtoupper($polymer[$i + 1])) {
                $polymer = substr($polymer, 0, $i) . substr($polymer, $i + 2);
                $reactionOccurred = true;
                break; // Restart the process after each reaction
            }
        }
    }
    return $polymer;
}

function main() {
    $content = file_get_contents("input.txt");
    if ($content === false) {
        echo "Error reading file.";
        return;
    }

    $polymer = trim($content);
    $minLength = strlen($polymer);

    for ($unit = ord('a'); $unit <= ord('z'); $unit++) {
        $unitChar = chr($unit);
        $tempPolymer = str_replace([$unitChar, strtoupper($unitChar)], '', $polymer);
        $reactedPolymer = react($tempPolymer);
        $reactedLength = strlen($reactedPolymer);
        if ($reactedLength < $minLength) {
            $minLength = $reactedLength;
        }
    }

    echo $minLength;
}

main();
?>