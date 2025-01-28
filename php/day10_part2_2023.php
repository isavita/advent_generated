
<?php

function solve() {
    $input = file("input.txt", FILE_IGNORE_NEW_LINES);
    $grid = [];
    $start = null;

    foreach ($input as $y => $line) {
        $grid[$y] = str_split($line);
        if (($x = array_search('S', $grid[$y])) !== false) {
            $start = [$x, $y];
        }
    }

    list($loop, $distances) = findLoop($grid, $start);

    $part1 = max($distances);

    $part2 = calculateEnclosedTiles($grid, $loop);

    echo "Part 1: " . $part1 . "\n";
    echo "Part 2: " . $part2 . "\n";
}

function findLoop($grid, $start) {
    $directions = [
        '|' => [[0, -1], [0, 1]],
        '-' => [[-1, 0], [1, 0]],
        'L' => [[0, -1], [1, 0]],
        'J' => [[0, -1], [-1, 0]],
        '7' => [[0, 1], [-1, 0]],
        'F' => [[0, 1], [1, 0]],
    ];

    $startDirections = [];
    foreach ([[0, -1], [0, 1], [-1, 0], [1, 0]] as $dir) {
        $nx = $start[0] + $dir[0];
        $ny = $start[1] + $dir[1];
        if (isset($grid[$ny][$nx])) {
          $neighbor = $grid[$ny][$nx];
          if (isset($directions[$neighbor])) {
            foreach($directions[$neighbor] as $nDir) {
              if ($nx + $nDir[0] === $start[0] && $ny + $nDir[1] === $start[1]) {
                $startDirections[] = $dir;
              }
            }
          }
        }
      }
      
      foreach ($directions as $pipe => $possibleDirections) {
        if ($possibleDirections == $startDirections || $possibleDirections == array_reverse($startDirections)) {
          $grid[$start[1]][$start[0]] = $pipe;
          break;
        }
      }

    $loop = [$start];
    $distances = [0];
    $current = $start;
    $previous = null;

    do {
        $pipe = $grid[$current[1]][$current[0]];
        $next = null;

        foreach ($directions[$pipe] as $dir) {
            $nx = $current[0] + $dir[0];
            $ny = $current[1] + $dir[1];

            if (isset($grid[$ny][$nx]) && ($previous === null || $nx !== $previous[0] || $ny !== $previous[1])) {
                $next = [$nx, $ny];
                break;
            }
        }

        $previous = $current;
        $current = $next;
        $loop[] = $current;
        $distances[] = count($distances);
    } while ($current !== $start);
    
    array_pop($loop);
    array_pop($distances);

    $loopDistances = [];
    foreach($loop as $i => $point) {
      $loopDistances[$point[1]][$point[0]] = min($distances[$i], count($distances) - $distances[$i]);
    }

    return [$loop, array_values($loopDistances)];
}

function calculateEnclosedTiles($grid, $loop) {
    $loopCoords = [];
    foreach($loop as $point) {
      $loopCoords[$point[1]][$point[0]] = true;
    }

    $enclosedCount = 0;
    foreach ($grid as $y => $row) {
        $inside = false;
        $lastCorner = '';
        for ($x = 0; $x < count($row); $x++) {
            if (isset($loopCoords[$y][$x])) {
                $pipe = $grid[$y][$x];
                if ($pipe === '|') {
                    $inside = !$inside;
                } elseif ($pipe !== '-') {
                    if ($lastCorner === '') {
                        $lastCorner = $pipe;
                    } else {
                      if ($lastCorner === 'F' && $pipe === 'J' || $lastCorner === 'L' && $pipe === '7'){
                        $inside = !$inside;
                      }
                      $lastCorner = '';
                    }
                }
            } elseif ($inside) {
                $enclosedCount++;
            }
        }
    }
    return $enclosedCount;
}

solve();
?>
