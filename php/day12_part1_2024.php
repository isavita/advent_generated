
<?php

function solve(array $grid): int {
    $rows = count($grid);
    if ($rows === 0) return 0;
    $cols = strlen($grid[0]);

    $visited = array_fill(0, $rows, array_fill(0, $cols, false));
    $totalPrice = 0;

    for ($r = 0; $r < $rows; $r++) {
        for ($c = 0; $c < $cols; $c++) {
            if (!$visited[$r][$c]) {
                [$area, $perimeter] = calculateRegion($grid, $r, $c, $visited);
                $totalPrice += $area * $perimeter;
            }
        }
    }
    return $totalPrice;
}

function calculateRegion(array $grid, int $row, int $col, array &$visited): array {
    $rows = count($grid);
    $cols = strlen($grid[0]);
    $char = $grid[$row][$col];
    $area = 0;
    $perimeter = 0;

    $queue = [[$row, $col]];
    $visited[$row][$col] = true;

    while (!empty($queue)) {
        [$x, $y] = array_shift($queue);
        $area++;
        $isBorder = ($x === 0 || $x === $rows - 1 || $y === 0 || $y === $cols - 1);

        $neighbors = [
            [$x - 1, $y], [$x + 1, $y], [$x, $y - 1], [$x, $y + 1]
        ];

        foreach ($neighbors as [$nx, $ny]) {
            if ($nx >= 0 && $nx < $rows && $ny >= 0 && $ny < $cols) {
                if ($grid[$nx][$ny] !== $char) {
                    $perimeter++;
                } elseif (!$visited[$nx][$ny]) {
                    $queue[] = [$nx, $ny];
                    $visited[$nx][$ny] = true;
                }
            } else if ($isBorder) {
                $perimeter++;
            }
        }
    }
    return [$area, $perimeter];
}

$grid = [];
$file = fopen("input.txt", "r");
if ($file) {
    while (($line = fgets($file)) !== false) {
        $grid[] = rtrim($line, "\r\n");
    }
    fclose($file);
}

echo solve($grid) . PHP_EOL;
?>
