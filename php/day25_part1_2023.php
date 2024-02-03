
<?php

$input = file_get_contents("input.txt");
$inputArray = explode("\n", trim($input));

$graph = [];

foreach ($inputArray as $line) {
    $parts = explode(": ", $line);
    $vertice = $parts[0];
    $others = explode(" ", $parts[1]);

    if (!array_key_exists($vertice, $graph)) {
        $graph[$vertice] = [];
    }

    foreach ($others as $other) {
        if (!array_key_exists($other, $graph)) {
            $graph[$other] = [];
        }

        $graph[$vertice][] = ['start' => $vertice, 'end' => $other, 'weight' => 1];
        $graph[$other][] = ['start' => $other, 'end' => $vertice, 'weight' => 1];
    }
}

function breadthFirstSearch($graph, $start, $goalFunc) {
    $frontier = [$start];
    $reached = [$start => true];
    $cameFrom = [$start => $start];

    while (!empty($frontier)) {
        $current = array_shift($frontier);

        if ($goalFunc($current)) {
            return [$cameFrom, true];
        }

        foreach ($graph[$current] as $next) {
            if (!array_key_exists($next['end'], $reached)) {
                $frontier[] = $next['end'];
                $reached[$next['end']] = true;
                $cameFrom[$next['end']] = $current;
            }
        }
    }

    return [$cameFrom, false];
}

function reconstructPath($start, $end, $cameFrom) {
    $path = [];
    $current = $end;
    while ($current != $start) {
        array_unshift($path, $current);
        $current = $cameFrom[$current];
    }
    array_unshift($path, $start);
    return $path;
}

function copyGraph($graph) {
    $newGraph = [];
    foreach ($graph as $vertice => $edges) {
        $newGraph[$vertice] = $edges;
    }
    return $newGraph;
}

function solve($input) {
    $minCut = 3;

    $graph = $input;

    $source = array_key_first($graph);

    $separateGraph = [];
    foreach ($graph as $end => $vertices) {
        if ($source == $end) {
            continue;
        }

        $newGraph = copyGraph($graph);
        for ($i = 0; $i < $minCut; $i++) {
            [$cameFrom, $isValid] = breadthFirstSearch($newGraph, $source, function ($vertice) use ($end) {
                return $vertice == $end;
            });

            $path = reconstructPath($source, $end, $cameFrom);
            $pathLength = count($path);
            for ($j = 0; $j < $pathLength - 1; $j++) {
                $edge = ['start' => $path[$j], 'end' => $path[$j+1], 'weight' => 1];
                $index = array_search($edge, $newGraph[$path[$j]]);
                unset($newGraph[$path[$j]][$index]);
            }
        }

        [$cameFrom, $isValid] = breadthFirstSearch($newGraph, $source, function ($vertice) use ($end) {
            return $vertice == $end;
        });

        if (!$isValid) {
            $separateGraph = $newGraph;
            break;
        }
    }

    [$cameFrom, $isValid] = breadthFirstSearch($separateGraph, $source, function ($vertice) {
        return false;
    });

    $lenght1 = count($cameFrom);
    $lenght2 = count($separateGraph) - $lenght1;

    return $lenght1 * $lenght2;
}

echo solve($graph);

?>
