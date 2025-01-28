
<?php

$graph = [];
$bestClique = [];

function BronKerbosch(array $R, array $P, array $X): void {
    global $bestClique, $graph;

    if (empty($P) && empty($X)) {
        if (count($R) > count($bestClique)) {
            $bestClique = $R;
        }
        return;
    }

    $tempP = $P;
    foreach ($tempP as $v) {
        $neighbors = neighborsOf($v);
        BronKerbosch(
            array_merge($R, [$v]),
            intersect($P, $neighbors),
            intersect($X, $neighbors)
        );
        $P = remove_func($P, $v);
        $X = array_merge($X, [$v]);
    }
}

function neighborsOf(string $node): array {
    global $graph;
    return isset($graph[$node]) ? array_keys($graph[$node]) : [];
}

function intersect(array $a, array $b_keys): array {
    $out = [];
    $b_set = array_flip($b_keys);
    foreach ($a as $x) {
        if (isset($b_set[$x])) {
            $out[] = $x;
        }
    }
    return $out;
}

function remove_func(array $slice, string $s): array {
    return array_values(array_filter($slice, function ($x) use ($s) {
        return $x !== $s;
    }));
}

$content = file_get_contents("input.txt");
$lines = explode("\n", $content);
$nodesSet = [];

foreach ($lines as $line) {
    $parts = explode("-", trim($line));
    if (count($parts) !== 2) {
        continue;
    }
    [$a, $b] = $parts;
    if (!isset($graph[$a])) {
        $graph[$a] = [];
    }
    if (!isset($graph[$b])) {
        $graph[$b] = [];
    }
    $graph[$a][$b] = true;
    $graph[$b][$a] = true;
    $nodesSet[$a] = true;
    $nodesSet[$b] = true;
}

$allNodes = array_keys($nodesSet);

BronKerbosch([], $allNodes, []);
sort($bestClique);
echo implode(",", $bestClique) . "\n";

?>
