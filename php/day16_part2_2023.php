
<?php

function solve($grid, $start_row, $start_col, $start_dir) {
    $rows = count($grid);
    $cols = count($grid[0]);
    $energized = array_fill(0, $rows, array_fill(0, $cols, false));
    $visited = [];
    $beams = [[$start_row, $start_col, $start_dir]];

    while (!empty($beams)) {
        $new_beams = [];
        foreach ($beams as [$row, $col, $dir]) {
            if ($row < 0 || $row >= $rows || $col < 0 || $col >= $cols) {
                continue;
            }
            $key = "$row,$col,$dir";
            if (isset($visited[$key])) {
                continue;
            }
            $visited[$key] = true;
            $energized[$row][$col] = true;

            $tile = $grid[$row][$col];
            switch ($tile) {
                case '.':
                    switch ($dir) {
                        case 'r': $new_beams[] = [$row, $col + 1, 'r']; break;
                        case 'l': $new_beams[] = [$row, $col - 1, 'l']; break;
                        case 'u': $new_beams[] = [$row - 1, $col, 'u']; break;
                        case 'd': $new_beams[] = [$row + 1, $col, 'd']; break;
                    }
                    break;
                case '/':
                    switch ($dir) {
                        case 'r': $new_beams[] = [$row - 1, $col, 'u']; break;
                        case 'l': $new_beams[] = [$row + 1, $col, 'd']; break;
                        case 'u': $new_beams[] = [$row, $col + 1, 'r']; break;
                        case 'd': $new_beams[] = [$row, $col - 1, 'l']; break;
                    }
                    break;
                case '\\':
                    switch ($dir) {
                        case 'r': $new_beams[] = [$row + 1, $col, 'd']; break;
                        case 'l': $new_beams[] = [$row - 1, $col, 'u']; break;
                        case 'u': $new_beams[] = [$row, $col - 1, 'l']; break;
                        case 'd': $new_beams[] = [$row, $col + 1, 'r']; break;
                    }
                    break;
                case '|':
                    switch ($dir) {
                        case 'r':
                        case 'l':
                            $new_beams[] = [$row - 1, $col, 'u'];
                            $new_beams[] = [$row + 1, $col, 'd'];
                            break;
                        case 'u': $new_beams[] = [$row - 1, $col, 'u']; break;
                        case 'd': $new_beams[] = [$row + 1, $col, 'd']; break;
                    }
                    break;
                case '-':
                    switch ($dir) {
                        case 'u':
                        case 'd':
                            $new_beams[] = [$row, $col - 1, 'l'];
                            $new_beams[] = [$row, $col + 1, 'r'];
                            break;
                        case 'r': $new_beams[] = [$row, $col + 1, 'r']; break;
                        case 'l': $new_beams[] = [$row, $col - 1, 'l']; break;
                    }
                    break;
            }
        }
        $beams = $new_beams;
    }

    $count = 0;
    foreach ($energized as $row) {
        foreach ($row as $cell) {
            if ($cell) {
                $count++;
            }
        }
    }
    return $count;
}

$input = file('input.txt', FILE_IGNORE_NEW_LINES);
$grid = array_map(fn($line) => str_split($line), $input);

// Part 1
$part1_result = solve($grid, 0, 0, 'r');
echo "Part 1: " . $part1_result . "\n";

// Part 2
$max_energized = 0;
$rows = count($grid);
$cols = count($grid[0]);

for ($i = 0; $i < $cols; $i++) {
    $max_energized = max($max_energized, solve($grid, 0, $i, 'd'));
    $max_energized = max($max_energized, solve($grid, $rows - 1, $i, 'u'));
}

for ($i = 0; $i < $rows; $i++) {
    $max_energized = max($max_energized, solve($grid, $i, 0, 'r'));
    $max_energized = max($max_energized, solve($grid, $i, $cols - 1, 'l'));
}

echo "Part 2: " . $max_energized . "\n";
?>
