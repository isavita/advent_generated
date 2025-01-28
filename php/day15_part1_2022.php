
<?php

function manhattan_distance($x1, $y1, $x2, $y2) {
    return abs($x1 - $x2) + abs($y1 - $y2);
}

function solve() {
    $lines = file('input.txt', FILE_IGNORE_NEW_LINES);
    if ($lines === false) {
        die("Error reading input.txt\n");
    }

    $sensors = [];
    foreach ($lines as $line) {
        preg_match('/Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)/', $line, $matches);
        if ($matches) {
            $sensors[] = [
                'sx' => intval($matches[1]),
                'sy' => intval($matches[2]),
                'bx' => intval($matches[3]),
                'by' => intval($matches[4]),
            ];
        }
    }

    $target_row = 2000000;
    $covered_ranges = [];

    foreach ($sensors as $sensor_data) {
        $distance = manhattan_distance($sensor_data['sx'], $sensor_data['sy'], $sensor_data['bx'], $sensor_data['by']);
        $dy = abs($sensor_data['sy'] - $target_row);
        $dx = max(0, $distance - $dy);
        if ($dx > 0 || $dx === 0 && $dy <= $distance) {
            $covered_ranges[] = [
                'start' => $sensor_data['sx'] - $dx,
                'end' => $sensor_data['sx'] + $dx,
            ];
        }
    }

    if (empty($covered_ranges)) {
        echo 0 . "\n";
        return;
    }

    usort($covered_ranges, function($a, $b) {
        return $a['start'] <=> $b['start'];
    });

    $merged_ranges = [];
    $current_range = $covered_ranges[0];

    for ($i = 1; $i < count($covered_ranges); $i++) {
        $next_range = $covered_ranges[$i];
        if ($next_range['start'] <= $current_range['end'] + 1) { // +1 for adjacent ranges to merge
            $current_range['end'] = max($current_range['end'], $next_range['end']);
        } else {
            $merged_ranges[] = $current_range;
            $current_range = $next_range;
        }
    }
    $merged_ranges[] = $current_range;

    $total_covered_positions = 0;
    foreach ($merged_ranges as $range) {
        $total_covered_positions += ($range['end'] - $range['start'] + 1);
    }

    $beacons_in_row = [];
    foreach ($sensors as $sensor_data) {
        if ($sensor_data['by'] == $target_row) {
            $beacons_in_row[$sensor_data['bx']] = true;
        }
    }

    $beacon_positions_count = 0;
    foreach ($beacons_in_row as $bx => $_) {
        foreach ($merged_ranges as $range) {
            if ($bx >= $range['start'] && $bx <= $range['end']) {
                $beacon_positions_count++;
                break;
            }
        }
    }

    echo $total_covered_positions - $beacon_positions_count . "\n";


}

solve();

?>
