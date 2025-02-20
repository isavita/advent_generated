
<?php
class Point {
    public $x;
    public $y;
    function __construct($x, $y) {
        $this->x = $x;
        $this->y = $y;
    }
}

class State {
    public $pos;
    public $keys;
    function __construct($pos, $keys) {
        $this->pos = $pos;
        $this->keys = $keys;
    }
    function hash() {
        return (($this->pos->x * 1000 + $this->pos->y) * 100000 + $this->keys);
    }
}

function find_shortest_path($grid, $start, $key_map) {
    $dirs = [new Point(0, -1), new Point(-1, 0), new Point(0, 1), new Point(1, 0)];
    $visited = [];
    $queue = new SplQueue();
    $queue->enqueue(new State($start, 0));
    $steps = 0;
    $target_keys = (1 << count($key_map)) - 1;

    while (!$queue->isEmpty()) {
        $size = $queue->count();
        for ($i = 0; $i < $size; $i++) {
            $current = $queue->dequeue();

            if ($current->keys == $target_keys) {
                return $steps;
            }

            foreach ($dirs as $d) {
                $next_pos = new Point($current->pos->x + $d->x, $current->pos->y + $d->y);
                if ($next_pos->x >= 0 && $next_pos->x < strlen($grid[0]) && $next_pos->y >= 0 && $next_pos->y < count($grid)) {
                    $char = $grid[$next_pos->y][$next_pos->x];
                    if ($char != '#') {
                        $is_upper = ctype_upper($char);
                        $lower_char = strtolower($char);
                        if (!$is_upper || ($current->keys & (1 << $key_map[$lower_char])))
                        {
                            $new_keys = $current->keys;

                            if (ctype_lower($char)) {
                                $new_keys |= (1 << $key_map[$char]);
                            }

                            $new_state = new State($next_pos, $new_keys);
                            $hash = $new_state->hash();

                            if (!isset($visited[$hash]))
                            {
                               $visited[$hash] = true;
                               $queue->enqueue($new_state);
                            }
                        }
                    }
                }
            }
        }
        $steps++;
    }
    return -1;
}

$file = fopen("input.txt", "r");
$grid = [];
$start = null;
$key_map = [];
$key_counter = 0;
$y = 0;

while (($line = fgets($file)) !== false) {
    $line = trim($line);
    $grid[] = $line;
    for ($x = 0; $x < strlen($line); $x++) {
        $char = $line[$x];
        if ($char == '@') {
            $start = new Point($x, $y);
        } elseif (ctype_lower($char)) {
            $key_map[$char] = $key_counter;
            $key_counter++;
        }
    }
    $y++;
}
fclose($file);

echo find_shortest_path($grid, $start, $key_map);

?>
