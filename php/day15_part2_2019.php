
<?php

class Machine {
    private array $data = [];
    private int $ip = 0;
    private array $input_queue = [];
    private array $output_queue = [];
    private int $relbase = 0;

    public function __construct(array $program) {
        $this->data = $program;
    }

    private function get(int $i, int $mo): int {
        return match ($mo) {
            0 => $this->data[$this->data[$i]] ?? 0,
            1 => $this->data[$i] ?? 0,
            2 => $this->data[$this->relbase + $this->data[$i]] ?? 0,
            default => throw new ValueError("Unknown mode: $mo"),
        };
    }

    private function set(int $i, int $mo, int $val): void {
        if ($mo === 0) {
            $this->data[$this->data[$i]] = $val;
        } elseif ($mo === 2) {
            $this->data[$this->relbase + $this->data[$i]] = $val;
        } else {
            throw new ValueError("Unknown mode: $mo");
        }
    }

    public function step(): bool {
        $op = $this->data[$this->ip] % 100;
        $modes = array_map(fn($i) => intdiv($this->data[$this->ip], 10**$i) % 10, range(2, 4));

        switch ($op) {
            case 1:
                $this->set($this->ip + 3, $modes[2], $this->get($this->ip + 1, $modes[0]) + $this->get($this->ip + 2, $modes[1]));
                $this->ip += 4;
                break;
            case 2:
                $this->set($this->ip + 3, $modes[2], $this->get($this->ip + 1, $modes[0]) * $this->get($this->ip + 2, $modes[1]));
                $this->ip += 4;
                break;
            case 3:
                if (empty($this->input_queue)) {
                    return false;
                }
                $this->set($this->ip + 1, $modes[0], array_shift($this->input_queue));
                $this->ip += 2;
                break;
            case 4:
                $this->output_queue[] = $this->get($this->ip + 1, $modes[0]);
                $this->ip += 2;
                break;
            case 5:
                $this->ip = ($this->get($this->ip + 1, $modes[0]) != 0) ? $this->get($this->ip + 2, $modes[1]) : $this->ip + 3;
                break;
            case 6:
                $this->ip = ($this->get($this->ip + 1, $modes[0]) == 0) ? $this->get($this->ip + 2, $modes[1]) : $this->ip + 3;
                break;
            case 7:
                $this->set($this->ip + 3, $modes[2], ($this->get($this->ip + 1, $modes[0]) < $this->get($this->ip + 2, $modes[1])) ? 1 : 0);
                $this->ip += 4;
                break;
            case 8:
                $this->set($this->ip + 3, $modes[2], ($this->get($this->ip + 1, $modes[0]) == $this->get($this->ip + 2, $modes[1])) ? 1 : 0);
                $this->ip += 4;
                break;
            case 9:
                $this->relbase += $this->get($this->ip + 1, $modes[0]);
                $this->ip += 2;
                break;
            case 99:
                return false;
            default:
                throw new ValueError("Unknown opcode: $op");
        }

        return true;
    }

    public function run(): void {
        while ($this->step()) {
        }
    }

    public function addInput(int $input): void {
        $this->input_queue[] = $input;
    }

    public function getOutput(): array {
        return $this->output_queue;
    }

    public function clearOutput(): void {
        $this->output_queue = [];
    }
}

class Pathfinder {
    private Machine $machine;
    private array $grid = [];
    private array $dirmap = [1 => [0, 1], 2 => [0, -1], 3 => [-1, 0], 4 => [1, 0]];
    private array $p = [0, 0];
    private ?array $oxygen = null;

    public function __construct(array $program) {
        $this->machine = new Machine($program);
        $this->grid[$this->posToString($this->p)] = '.';
    }

    private function posToString(array $pos): string {
        return $pos[0] . ',' . $pos[1];
    }

    private function stringToPos(string $str): array {
        return array_map('intval', explode(',', $str));
    }

    private function tryMove(int $dir): bool {
        $this->machine->clearOutput();
        $this->machine->addInput($dir);
        $this->machine->run();
        $output = $this->machine->getOutput();

        if (empty($output)) {
            return false;
        }

        $output = $output[0];
        $nextPos = [$this->p[0] + $this->dirmap[$dir][0], $this->p[1] + $this->dirmap[$dir][1]];
        $nextPosStr = $this->posToString($nextPos);

        if ($output === 0) {
            $this->grid[$nextPosStr] = '#';
            return false;
        } elseif ($output === 1) {
            $this->grid[$nextPosStr] = '.';
        } elseif ($output === 2) {
            $this->grid[$nextPosStr] = 'O';
            $this->oxygen = $nextPos;
        }

        $this->p = $nextPos;
        return true;
    }

    public function explore(): void {
        while ($this->open()) {
            $openPositions = $this->open();
            $currentPosStr = $this->posToString($this->p);
            if (!in_array($currentPosStr, $openPositions, true)) {
                $minDist = PHP_FLOAT_MAX;
                $nextPos = null;

                foreach ($openPositions as $posStr) {
                    $pos = $this->stringToPos($posStr);
                    $dist = abs($this->p[0] - $pos[0]) + abs($this->p[1] - $pos[1]);
                    if ($dist < $minDist) {
                        $minDist = $dist;
                        $nextPos = $pos;
                    }
                }
                $path = $this->shortestPath($this->p, $nextPos);

                foreach ($path as $move) {
                    if (!$this->tryMove($move)) {
                        throw new ValueError("Bad path");
                    }
                }
            }

            while (true) {
                $foundUnexplored = false;
                foreach ($this->dirmap as $dir => $delta) {
                    $nextPos = [$this->p[0] + $delta[0], $this->p[1] + $delta[1]];
                    $nextPosStr = $this->posToString($nextPos);

                    if (!array_key_exists($nextPosStr, $this->grid)) {
                        $foundUnexplored = true;
                        break;
                    }
                }

                if (!$foundUnexplored) {
                    break;
                }
                if (!$this->tryMove($dir)) {
                    break;
                }
            }
        }
    }

    private function open(): array {
        $open = [];
        foreach ($this->grid as $posStr => $val) {
            $pos = $this->stringToPos($posStr);
            if ($val !== '#') {
                foreach ($this->dirmap as $delta) {
                    $neighbor = [$pos[0] + $delta[0], $pos[1] + $delta[1]];
                    $neighborStr = $this->posToString($neighbor);
                    if (!array_key_exists($neighborStr, $this->grid)) {
                        $open[] = $posStr;
                        break;
                    }
                }
            }
        }
        return $open;
    }

    private function shortestPath(array $start, array $end): array {
        $queue = [[0, $start, []]];
        $visited = [];

        while (!empty($queue)) {
            [$dist, $pos, $path] = array_shift($queue);
            if ($pos === $end) {
                return $path;
            }

            $posStr = $this->posToString($pos);
            if (in_array($posStr, $visited, true)) {
                continue;
            }
            $visited[] = $posStr;

            foreach ($this->dirmap as $dir => $delta) {
                $nextPos = [$pos[0] + $delta[0], $pos[1] + $delta[1]];
                $nextPosStr = $this->posToString($nextPos);
                if (array_key_exists($nextPosStr, $this->grid) && $this->grid[$nextPosStr] !== '#') {
                    $newQueueItem = [$dist + 1, $nextPos, array_merge($path, [$dir])];

                    $inserted = false;
                    for ($i = 0; $i < count($queue); $i++) {
                        if ($newQueueItem[0] < $queue[$i][0]) {
                            array_splice($queue, $i, 0, [$newQueueItem]);
                            $inserted = true;
                            break;
                        }
                    }
                    if (!$inserted) {
                        $queue[] = $newQueueItem;
                    }
                }
            }
        }

        throw new ValueError("No path found");
    }

    public function longestPath(array $start): int {
        $queue = [[0, $start]];
        $distances = [$this->posToString($start) => 0];

        while (!empty($queue)) {
            [$dist, $pos] = array_shift($queue);
            foreach ($this->dirmap as $delta) {
                $nextPos = [$pos[0] + $delta[0], $pos[1] + $delta[1]];
                $nextPosStr = $this->posToString($nextPos);

                if (array_key_exists($nextPosStr, $this->grid) && $this->grid[$nextPosStr] !== '#' && (
                        !array_key_exists($nextPosStr, $distances) || $distances[$nextPosStr] > $dist + 1)) {
                    $distances[$nextPosStr] = $dist + 1;
                    $queue[] = [$dist + 1, $nextPos];

                     usort($queue, function($a, $b) {
                        return $a[0] <=> $b[0];
                    });
                }
            }
        }

        return max($distances);
    }

    public function getOxygen(): ?array {
        return $this->oxygen;
    }
}

$program = array_map('intval', explode(',', trim(file_get_contents("input.txt"))));

$pathfinder = new Pathfinder($program);
$pathfinder->explore();
$oxygen = $pathfinder->getOxygen();
echo $pathfinder->longestPath($oxygen) . PHP_EOL;
