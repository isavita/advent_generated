
<?php

$input = file_get_contents("input.txt");
$moves = explode(",", $input);

$programs = str_split("abcdefghijklmnop");
$initial = implode("", $programs);
$cycleLen = 0;

for ($i = 0; $i < 1000000000; $i++) {
    foreach ($moves as $move) {
        switch ($move[0]) {
            case 's':
                $x = (int)substr($move, 1);
                spin($programs, $x);
                break;
            case 'x':
                $positions = explode("/", substr($move, 1));
                $A = (int)$positions[0];
                $B = (int)$positions[1];
                exchange($programs, $A, $B);
                break;
            case 'p':
                $positions = explode("/", substr($move, 1));
                $A = $positions[0];
                $B = $positions[1];
                partner($programs, $A, $B);
                break;
        }
    }

    if (implode("", $programs) == $initial) {
        $cycleLen = $i + 1;
        break;
    }
}

$programs = str_split($initial);

for ($i = 0; $i < 1000000000 % $cycleLen; $i++) {
    foreach ($moves as $move) {
        switch ($move[0]) {
            case 's':
                $x = (int)substr($move, 1);
                spin($programs, $x);
                break;
            case 'x':
                $positions = explode("/", substr($move, 1));
                $A = (int)$positions[0];
                $B = (int)$positions[1];
                exchange($programs, $A, $B);
                break;
            case 'p':
                $positions = explode("/", substr($move, 1));
                $A = $positions[0];
                $B = $positions[1];
                partner($programs, $A, $B);
                break;
        }
    }
}

echo implode("", $programs) . PHP_EOL;

function spin(&$programs, $x) {
    $n = count($programs);
    $temp = $programs;

    for ($i = 0; $i < $n; $i++) {
        $programs[($i + $x) % $n] = $temp[$i];
    }
}

function exchange(&$programs, $A, $B) {
    list($programs[$A], $programs[$B]) = array($programs[$B], $programs[$A]);
}

function partner(&$programs, $A, $B) {
    $indexA = array_search($A, $programs);
    $indexB = array_search($B, $programs);
    exchange($programs, $indexA, $indexB);
}
?>
