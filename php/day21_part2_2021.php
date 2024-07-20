
<?php

function main() {
    $input = trim(file_get_contents("input.txt"));
    $positions = parseInput($input);
    $result = solve($positions);
    echo $result;
}

function solve($positions) {
    $memo = [];
    list($w1, $w2) = play([$positions[0], $positions[1]], [0, 0], 3, true, $memo);
    return max($w1, $w2);
}

function play($positions, $scores, $rollsLeftInTurn, $isPlayer1sTurn, &$memo) {
    $key = implode(',', $positions) . ',' . implode(',', $scores) . ',' . $rollsLeftInTurn . ',' . (int)$isPlayer1sTurn;
    if (isset($memo[$key])) {
        return $memo[$key];
    }

    $playerIndex = $isPlayer1sTurn ? 0 : 1;
    $scoresCopy = $scores;

    if ($rollsLeftInTurn === 0) {
        $scoresCopy[$playerIndex] += $positions[$playerIndex];
        if ($scoresCopy[$playerIndex] >= 21) {
            return $playerIndex === 0 ? [1, 0] : [0, 1];
        }
        $isPlayer1sTurn = !$isPlayer1sTurn;
        $rollsLeftInTurn = 3;
        $playerIndex = 1 - $playerIndex;
    }

    $wins1 = $wins2 = 0;
    for ($roll = 1; $roll <= 3; $roll++) {
        $positionsCopy = $positions;
        $positionsCopy[$playerIndex] += $roll;
        if ($positionsCopy[$playerIndex] > 10) {
            $positionsCopy[$playerIndex] -= 10;
        }
        list($r1, $r2) = play($positionsCopy, $scoresCopy, $rollsLeftInTurn - 1, $isPlayer1sTurn, $memo);
        $wins1 += $r1;
        $wins2 += $r2;
    }

    return $memo[$key] = [$wins1, $wins2];
}

function parseInput($input) {
    preg_match_all('/Player (\d+) starting position: (\d+)/', $input, $matches);
    return array_map('intval', $matches[2]);
}

main();
