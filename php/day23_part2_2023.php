
<?php

function solve(string $inputFile, bool $ignoreSlopes): int
{
    $grid = array_map('str_split', file($inputFile, FILE_IGNORE_NEW_LINES));
    $rows = count($grid);
    $cols = count($grid[0]);
    $start = [0, array_search('.', $grid[0])];
    $end = [$rows - 1, array_search('.', $grid[$rows - 1])];

    $graph = buildGraph($grid, $ignoreSlopes);

    return findLongestPath($graph, $start, $end);
}

function buildGraph(array $grid, bool $ignoreSlopes): array
{
    $rows = count($grid);
    $cols = count($grid[0]);
    $graph = [];
    $directions = [[-1, 0], [1, 0], [0, -1], [0, 1]];
    $slopes = ['^' => [-1, 0], 'v' => [1, 0], '<' => [0, -1], '>' => [0, 1]];

    for ($r = 0; $r < $rows; $r++) {
        for ($c = 0; $c < $cols; $c++) {
            if ($grid[$r][$c] === '#') continue;

            $neighbors = [];
            foreach ($directions as $dir) {
                if (!$ignoreSlopes && isset($slopes[$grid[$r][$c]]) && $slopes[$grid[$r][$c]] !== $dir) continue;

                $nr = $r + $dir[0];
                $nc = $c + $dir[1];

                if ($nr >= 0 && $nr < $rows && $nc >= 0 && $nc < $cols && $grid[$nr][$nc] !== '#') {
                    $neighbors[] = [$nr, $nc];
                }
            }
            $graph[posToKey($r, $c)] = $neighbors;
        }
    }
    return compressGraph($graph);
}

function compressGraph(array $graph): array
{
    $compressedGraph = [];

    foreach ($graph as $node => $neighbors) {
        $compressedGraph[$node] = [];
        foreach ($neighbors as $neighbor) {
            $dist = 1;
            $prev = keyToPos($node);
            $curr = $neighbor;

            while (count($graph[posToKey($curr[0], $curr[1])]) === 2) {
                $nextNeighbors = $graph[posToKey($curr[0], $curr[1])];
                $next = ($nextNeighbors[0] == $prev) ? $nextNeighbors[1] : $nextNeighbors[0];
                $prev = $curr;
                $curr = $next;
                $dist++;
            }
            $compressedGraph[$node][posToKey($curr[0], $curr[1])] = $dist;
        }
    }

    return $compressedGraph;
}

function findLongestPath(array $graph, array $start, array $end): int
{
    $longestPath = 0;
    $stack = [[posToKey($start[0], $start[1]), 0, []]];

    while (!empty($stack)) {
        [$curr, $dist, $visited] = array_pop($stack);

        if ($curr === posToKey($end[0], $end[1])) {
            $longestPath = max($longestPath, $dist);
            continue;
        }

        $visited[$curr] = true;

        foreach ($graph[$curr] as $neighbor => $weight) {
            if (!isset($visited[$neighbor])) {
                $stack[] = [$neighbor, $dist + $weight, $visited];
            }
        }
    }

    return $longestPath;
}

function posToKey(int $row, int $col): string
{
    return "$row,$col";
}

function keyToPos(string $key): array
{
    return explode(',', $key);
}

$inputFile = 'input.txt';
echo "Part 1: " . solve($inputFile, false) . "\n";
echo "Part 2: " . solve($inputFile, true) . "\n";

?>
