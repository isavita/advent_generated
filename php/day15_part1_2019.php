
<?php

class IntcodeComputer
{
    private array $memory;
    private int $ip = 0;
    private int $relativeBase = 0;
    private array $inputs = [];
    private array $outputs = [];

    public function __construct(array $program)
    {
        $this->memory = $program;
    }

    public function addInput(int $input): void
    {
        $this->inputs[] = $input;
    }

    public function getOutput(): ?int
    {
        return array_shift($this->outputs);
    }

    public function run(): bool
    {
        while ($this->ip < count($this->memory)) {
            $opcode = $this->memory[$this->ip] % 100;
            $modes = array_map('intval', str_split((string)floor($this->memory[$this->ip] / 100), 1));
            $modes = array_reverse($modes);
            $modes = array_pad($modes, 3, 0);

            $getParam = function (int $offset) use ($modes): int {
                $mode = $modes[$offset - 1];
                $value = $this->memory[$this->ip + $offset];
                if ($mode === 0) {
                    return $this->memory[$value] ?? 0;
                } elseif ($mode === 1) {
                    return $value;
                } elseif ($mode === 2) {
                    return $this->memory[$this->relativeBase + $value] ?? 0;
                }
                return 0;
            };

            $getDestination = function (int $offset) use ($modes): int {
                $mode = $modes[$offset - 1];
                $value = $this->memory[$this->ip + $offset];
                if ($mode === 0) {
                    return $value;
                } elseif ($mode === 2) {
                    return $this->relativeBase + $value;
                }
                return 0;
            };

            if ($opcode === 1) {
                $this->memory[$getDestination(3)] = $getParam(1) + $getParam(2);
                $this->ip += 4;
            } elseif ($opcode === 2) {
                $this->memory[$getDestination(3)] = $getParam(1) * $getParam(2);
                $this->ip += 4;
            } elseif ($opcode === 3) {
                if (empty($this->inputs)) {
                    return false;
                }
                $this->memory[$getDestination(1)] = array_shift($this->inputs);
                $this->ip += 2;
            } elseif ($opcode === 4) {
                $this->outputs[] = $getParam(1);
                $this->ip += 2;
            } elseif ($opcode === 5) {
                if ($getParam(1) !== 0) {
                    $this->ip = $getParam(2);
                } else {
                    $this->ip += 3;
                }
            } elseif ($opcode === 6) {
                if ($getParam(1) === 0) {
                    $this->ip = $getParam(2);
                } else {
                    $this->ip += 3;
                }
            } elseif ($opcode === 7) {
                $this->memory[$getDestination(3)] = ($getParam(1) < $getParam(2)) ? 1 : 0;
                $this->ip += 4;
            } elseif ($opcode === 8) {
                $this->memory[$getDestination(3)] = ($getParam(1) === $getParam(2)) ? 1 : 0;
                $this->ip += 4;
            } elseif ($opcode === 9) {
                $this->relativeBase += $getParam(1);
                $this->ip += 2;
            } elseif ($opcode === 99) {
                return true;
            }
        }
        return true;
    }
}

$program = array_map('intval', explode(',', trim(file_get_contents('input.txt'))));

$directions = [
    1 => [0, -1], // North
    2 => [0, 1],  // South
    3 => [-1, 0], // West
    4 => [1, 0],  // East
];

$map = [];
$queue = new SplQueue();
$queue->enqueue([[0, 0], new IntcodeComputer($program), []]);
$visited = [];
$oxygenSystemLocation = null;

while (!$queue->isEmpty()) {
    [$currentPos, $computer, $path] = $queue->dequeue();
    $x = $currentPos[0];
    $y = $currentPos[1];

    if (isset($visited["$x,$y"])) {
        continue;
    }
    $visited["$x,$y"] = true;

    foreach ($directions as $direction => $move) {
        $newX = $x + $move[0];
        $newY = $y + $move[1];
        $newComputer = clone $computer;
        $newComputer->addInput($direction);
        $newComputer->run();
        $status = $newComputer->getOutput();

        if ($status === 0) {
            $map["$newX,$newY"] = '#';
        } elseif ($status === 1) {
            $map["$newX,$newY"] = '.';
            $queue->enqueue([[$newX, $newY], $newComputer, array_merge($path, [$direction])]);
        } elseif ($status === 2) {
            $oxygenSystemLocation = [$newX, $newY];
            $map["$newX,$newY"] = 'O';
            $queue->enqueue([[$newX, $newY], $newComputer, array_merge($path, [$direction])]);
        }
    }
}

$start = [0, 0];
$queue = new SplQueue();
$queue->enqueue([$start, 0]);
$visited = [];
$minSteps = 0;

while (!$queue->isEmpty()) {
    [$current, $steps] = $queue->dequeue();
    $x = $current[0];
    $y = $current[1];

    if (isset($visited["$x,$y"])) {
        continue;
    }
    $visited["$x,$y"] = true;

    if ($map["$x,$y"] === 'O') {
        $minSteps = $steps;
        break;
    }

    foreach ($directions as $move) {
        $newX = $x + $move[0];
        $newY = $y + $move[1];
        if (isset($map["$newX,$newY"]) && $map["$newX,$newY"] !== '#') {
            $queue->enqueue([[$newX, $newY], $steps + 1]);
        }
    }
}

echo $minSteps . PHP_EOL;
