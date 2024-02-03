
<?php

const Size = 10007;

$deck = range(0, Size - 1);

$file = fopen("input.txt", "r");
if ($file) {
    while (($line = fgets($file)) !== false) {
        if (trim($line) === "deal into new stack") {
            $deck = dealIntoNewStack($deck);
            continue;
        }

        if (strpos($line, "cut") === 0) {
            $n = (int) explode(" ", $line)[1];
            $deck = cutN($deck, $n);
            continue;
        }

        if (strpos($line, "deal with increment") === 0) {
            $n = (int) explode(" ", $line)[count(explode(" ", $line)) - 1];
            $deck = dealWithIncrement($deck, $n);
            continue;
        }
    }

    fclose($file);
}

echo find2019($deck) . PHP_EOL;

function dealIntoNewStack($deck) {
    $newDeck = $deck;
    for ($i = 0; $i < Size / 2; $i++) {
        $newDeck[$i] = $deck[Size - $i - 1];
        $newDeck[Size - $i - 1] = $deck[$i];
    }
    return $newDeck;
}

function cutN($deck, $n) {
    if ($n >= 0) {
        return array_merge(array_slice($deck, $n), array_slice($deck, 0, $n));
    } else {
        return array_merge(array_slice($deck, count($deck) + $n), array_slice($deck, 0, count($deck) + $n));
    }
}

function dealWithIncrement($deck, $n) {
    $newDeck = array_fill(0, Size, 0);

    for ($i = 0; $i < Size; $i++) {
        $newDeck[($i * $n) % Size] = $deck[$i];
    }

    return $newDeck;
}

function find2019($deck) {
    for ($i = 0; $i < Size; $i++) {
        if ($deck[$i] === 2019) {
            return $i;
        }
    }
    return -1;
}
?>
