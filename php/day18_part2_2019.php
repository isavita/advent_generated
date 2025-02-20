
<?php

function solve() {
    $original_map = array_map(function($line) { return str_split(trim($line)); }, file("input.txt", FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES));

    $found = false;
    for ($y = 1; $y < count($original_map) - 1; $y++) {
        for ($x = 1; $x < count($original_map[0]) - 1; $x++) {
            if ($original_map[$y][$x] == '@') {
                if ($original_map[$y-1][$x] == '.' && $original_map[$y+1][$x] == '.' &&
                    $original_map[$y][$x-1] == '.' && $original_map[$y][$x+1] == '.') {
                    $original_map[$y-1][$x-1] = '@'; $original_map[$y-1][$x] = '#'; $original_map[$y-1][$x+1] = '@';
                    $original_map[$y][$x-1] = '#'; $original_map[$y][$x] = '#'; $original_map[$y][$x+1] = '#';
                    $original_map[$y+1][$x-1] = '@'; $original_map[$y+1][$x] = '#'; $original_map[$y+1][$x+1] = '@';
                    $found = true;
                    break;
                }
            }
        }
        if ($found) break;
    }

    $robot_positions = [];
    foreach ($original_map as $y => $row) {
        foreach ($row as $x => $cell) {
            if ($cell == '@') {
                $robot_positions[] = [$x, $y];
            }
        }
    }

    $keys = [];
    $doors = [];
    $all_keys = [];
    foreach ($original_map as $y => $row) {
        foreach ($row as $x => $cell) {
            if (ctype_lower($cell)) {
                $keys[$cell] = [$x, $y];
                $all_keys[] = $cell;
            } elseif (ctype_upper($cell)) {
                $doors[$cell] = [$x, $y];
            }
        }
    }

    function bfs($start_pos, &$original_map) {
        $queue = new SplQueue();
        $queue->enqueue([$start_pos[0], $start_pos[1], 0, []]);
        $visited = [];
        $results = [];

        while (!$queue->isEmpty()) {
            [$x, $y, $dist, $required_keys] = $queue->dequeue();
            $visited_key = $x . ',' . $y;
            if (isset($visited[$visited_key])) continue;
            $visited[$visited_key] = true;

            $cell = $original_map[$y][$x];
            if (ctype_lower($cell) && !in_array($cell, $required_keys)) {
                $results[$cell] = [$dist, $required_keys];
                $required_keys = array_unique(array_merge($required_keys, [$cell]));
            }

            $moves = [[-1,0], [1,0], [0,-1], [0,1]];
            foreach ($moves as [$dx, $dy]) {
                $nx = $x + $dx;
                $ny = $y + $dy;
                if ($ny >= 0 && $ny < count($original_map) && $nx >= 0 && $nx < count($original_map[0])) {
                    $ncell = $original_map[$ny][$nx];
                    if ($ncell != '#') {
                        if (ctype_upper($ncell)) {
                            $n_required_keys = array_unique(array_merge($required_keys, [strtolower($ncell)]));
                            $queue->enqueue([$nx, $ny, $dist+1, $n_required_keys]);
                        } else {
                            $queue->enqueue([$nx, $ny, $dist+1, $required_keys]);
                        }
                    }
                }
            }
        }
        return $results;
    }

    $robot_reachable_keys = [];
    foreach ($robot_positions as $pos) {
        $robot_reachable_keys[] = bfs($pos, $original_map);
    }
    
    $key_positions = $keys;
    foreach ($robot_positions as $i => $pos) {
        $key_positions['@' . $i] = $pos;
    }

      function key_bfs($start_key, &$original_map, &$key_positions) {
        $start_pos = $key_positions[$start_key];
        $queue = new SplQueue();
        $queue->enqueue([$start_pos[0], $start_pos[1], 0, []]); 
        $visited = [];
        $results = [];

        while (!$queue->isEmpty()) {
            [$x, $y, $dist, $required_keys] = $queue->dequeue();
             $visited_key = $x . ',' . $y;
            if (isset($visited[$visited_key])) continue;
            $visited[$visited_key] = true;


            $cell = $original_map[$y][$x];
            if (ctype_lower($cell) && $cell != strtolower($start_key) && !in_array($cell,$required_keys) )
             {
                $results[$cell] = [$dist, $required_keys];
                 $required_keys = array_unique(array_merge($required_keys, [$cell]));
            }

            $moves = [[-1,0], [1,0], [0,-1], [0,1]];
            foreach ($moves as [$dx, $dy]) {
                $nx = $x + $dx;
                $ny = $y + $dy;

                if ($ny >= 0 && $ny < count($original_map) && $nx >= 0 && $nx < count($original_map[0])) {
                    $ncell = $original_map[$ny][$nx];
                    if ($ncell != '#') {
                        if (ctype_upper($ncell)) {
                            $n_required_keys =  array_unique(array_merge($required_keys, [strtolower($ncell)]));
                            $queue->enqueue([$nx, $ny, $dist+1, $n_required_keys]);
                        } else {
                            $queue->enqueue([$nx, $ny, $dist+1, $required_keys]);
                        }
                    }
                }
            }
        }
        return $results;
    }
    
    $key_graph = [];
    $all_nodes = array_keys($keys);
    foreach($robot_positions as $k => $v)
        $all_nodes[] = "@".$k;
    
    foreach ($all_nodes as $key) {
        $key_graph[$key] = key_bfs($key, $original_map, $key_positions);
    }


    function bitmask($keys_set) {
        $mask = 0;
        foreach ($keys_set as $k) {
            $mask |= 1 << (ord($k) - ord('a'));
        }
        return $mask;
    }

    function dijkstra($key_graph, $robot_positions, $all_keys) {
        $total_keys = count($all_keys);
        $initial_positions = [];
        for($i=0; $i<count($robot_positions); ++$i)
            $initial_positions[]="@".$i;
        
        $initial_state = [$initial_positions, []];
        $heap = new SplPriorityQueue();
        $heap->insert([$initial_positions, []], 0);
        $visited = [];

        while (!$heap->isEmpty()) {
             $heap->setExtractFlags(SplPriorityQueue::EXTR_BOTH);
            $current = $heap->extract();
            $cost = -$current['priority'];
            $positions = $current['data'][0];
            $collected_keys = $current['data'][1];


            $state = [implode(",", $positions), bitmask($collected_keys)];
            $state_key = implode("|", $state);

            if (isset($visited[$state_key]) && $visited[$state_key] <= $cost) continue;
            $visited[$state_key] = $cost;

            if (count($collected_keys) == $total_keys) return $cost;

            for ($i = 0; $i < count($positions); $i++) {
                $pos = $positions[$i];
                $available_keys = [];
                foreach ($key_graph[$pos] as $key => $data) {
                    $dist = $data[0];
                    $required_keys = $data[1];

                    if (!in_array($key, $collected_keys) && count(array_diff($required_keys, $collected_keys)) === 0 ) {
                        $available_keys[] = [$key, $dist];
                    }
                }

                foreach ($available_keys as [$key, $dist]) {
                    $new_positions = $positions;
                    $new_positions[$i] = $key;
                    $new_collected_keys = array_unique(array_merge($collected_keys, [$key]));
                    $new_state = [implode(",", $new_positions), bitmask($new_collected_keys)];
                    $new_state_key = implode("|", $new_state);
                    if(isset($visited[$new_state_key]) && $visited[$new_state_key] <= $cost+$dist)
                        continue;

                     $heap->insert([$new_positions, $new_collected_keys], -($cost + $dist));

                }
            }
        }
        return null;
    }

    return dijkstra($key_graph, $robot_positions, $all_keys);
}

echo solve();

?>
