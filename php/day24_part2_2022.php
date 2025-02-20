
<?php

function solveBlizzardBasin(string $filename, bool $partTwo = false): int
{
    $lines = file($filename, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
    $grid = array_map('str_split', $lines);
    $rows = count($grid);
    $cols = count($grid[0]);
    $start = [0, 1];
    $end = [$rows - 1, $cols - 2];

    $blizzards = [];
    for ($r = 0; $r < $rows; $r++) {
        for ($c = 0; $c < $cols; $c++) {
            if (in_array($grid[$r][$c], ['>', '<', '^', 'v'])) {
                $blizzards[] = [$r, $c, $grid[$r][$c]];
            }
        }
    }

    $trips = [[$start, $end], [$end, $start], [$start, $end]];
    $totalMinutes = 0;

    for ($tripIndex = 0; $tripIndex < ($partTwo ? 3 : 1); $tripIndex++) {
      $currentStart = $trips[$tripIndex][0];
      $currentEnd = $trips[$tripIndex][1];
      
      $q = [[$currentStart[0], $currentStart[1], $totalMinutes]];  // row, col, minutes
      $visited = [];
      $visited["{$currentStart[0]},{$currentStart[1]},{$totalMinutes}"] = true;

      while (!empty($q)) {
          [$r, $c, $minutes] = array_shift($q);

          if ($r == $currentEnd[0] && $c == $currentEnd[1]) {
              $totalMinutes = $minutes;
              break; 
          }

          $nextMinutes = $minutes + 1;
          $nextBlizzards = getBlizzardPositions($blizzards, $rows, $cols, $nextMinutes);

          $moves = [[0, 0], [0, 1], [0, -1], [1, 0], [-1, 0]]; // wait, right, left, down, up
          foreach ($moves as $move) {
              $nr = $r + $move[0];
              $nc = $c + $move[1];

              if (($nr >= 0 && $nr < $rows && $nc >= 0 && $nc < $cols && $grid[$nr][$nc] != '#') &&
                 !in_array([$nr, $nc], $nextBlizzards)) {
                
                  $key = "$nr,$nc,$nextMinutes";

                  if (!isset($visited[$key]))
                  {
                    $visited[$key] = true;
                    $q[] = [$nr, $nc, $nextMinutes];
                  }
              }
          }
      }
    }

    return $totalMinutes;
}


function getBlizzardPositions(array $blizzards, int $rows, int $cols, int $minutes): array
{
    $positions = [];
    foreach ($blizzards as $blizzard) {
        [$r, $c, $dir] = $blizzard;
        $newR = $r;
        $newC = $c;

        switch ($dir) {
            case '>':
                $newC = 1 + (($c - 1 + $minutes) % ($cols - 2));
                break;
            case '<':
                $newC = 1 + (($c - 1 - $minutes) % ($cols - 2));
                if ($newC < 1) {
                   $newC += ($cols - 2); 
                }
                break;
            case 'v':
                $newR = 1 + (($r - 1 + $minutes) % ($rows - 2));
                break;
            case '^':
                $newR = 1 + (($r - 1 - $minutes) % ($rows - 2));
                 if ($newR < 1) {
                   $newR += ($rows - 2); 
                }
                break;
        }
      $positions[] = [$newR, $newC];
    }
    return $positions;
}


$part1Result = solveBlizzardBasin("input.txt");
echo "Part 1: " . $part1Result . "\n";

$part2Result = solveBlizzardBasin("input.txt", true);
echo "Part 2: " . $part2Result . "\n";

?>
