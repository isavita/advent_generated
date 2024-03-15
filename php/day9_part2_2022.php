<?php
function visited($input, $ropelen) {
    $rope = array_fill(0, $ropelen, array(0, 0));
    $visited = array();
    $visited[implode(',', $rope[count($rope) - 1])] = true;
    foreach (explode("\n", $input) as $line) {
        list($b, $n) = sscanf($line, "%c %d");
        $d = dirFromByte($b);
        for ($i = 0; $i < $n; $i++) {
            $rope[0][0] += $d[0];
            $rope[0][1] += $d[1];
            for ($j = 1; $j < $ropelen; $j++) {
                $rope[$j] = moveRope($rope[$j - 1], $rope[$j]);
            }
            $visited[implode(',', $rope[$ropelen - 1])] = true;
        }
    }
    return count($visited);
}

function moveRope($head, $tail) {
    $dx = $head[0] - $tail[0];
    $dy = $head[1] - $tail[1];
    if (abs($dx) <= 1 && abs($dy) <= 1) {
        return $tail;
    }
    return array($tail[0] + sign($dx), $tail[1] + sign($dy));
}

function sign($n) {
    if ($n == 0) {
        return 0;
    }
    return ($n > 0) ? 1 : -1;
}

function dirFromByte($b) {
    $fromByte = array(
        'N' => array(0, 1),
        'E' => array(1, 0),
        'S' => array(0, -1),
        'W' => array(-1, 0),
        'U' => array(0, 1),
        'R' => array(1, 0),
        'D' => array(0, -1),
        'L' => array(-1, 0),
        '^' => array(0, 1),
        '>' => array(1, 0),
        'v' => array(0, -1),
        '<' => array(-1, 0),
    );
    return $fromByte[$b];
}

$input = file_get_contents('input.txt');
echo visited($input, 10);