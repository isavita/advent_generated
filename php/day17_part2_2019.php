
<?php

function readAll(string $filepath): string {
    return trim(file_get_contents($filepath));
}

function Atoi(string $s): int {
    return intval($s);
}

function decode(int $n): array {
    $op = $n % 100;
    $n = intdiv($n, 100);
    $modes = [];
    for ($i = 0; $i < 3; $i++) {
        $modes[$i] = $n % 10;
        $n = intdiv($n, 10);
    }
    return [$op, $modes];
}

class Machine {
    public array $data;
    public int $ip = 0;
    public $in;
    public $out;
    public int $relbase = 0;

    public function __construct(array $program, $in, $out) {
        $this->data = $program;
        $this->in = $in;
        $this->out = $out;
    }

    public function get(int $i, int $mo): int {
        switch ($mo) {
            case 1:
                return $this->data[$i];
            case 0:
                return $this->data[$this->data[$i]] ?? 0;
            case 2:
                return $this->data[$this->relbase + ($this->data[$i] ?? 0)] ?? 0;
            default:
                throw new Exception("Unknown mode: $mo");
        }
    }

    public function set(int $i, int $mo, int $val): void {
        switch ($mo) {
            case 0:
                $this->data[$this->data[$i]] = $val;
                break;
            case 2:
                $this->data[$this->relbase + ($this->data[$i] ?? 0)] = $val;
                break;
            default:
                throw new Exception("Unknown mode: $mo");
        }
    }

    public function step(): bool {
        [$op, $modes] = decode($this->data[$this->ip] ?? 0);
        switch ($op) {
            case 1:
                $val = $this->get($this->ip + 1, $modes[0]) + $this->get($this->ip + 2, $modes[1]);
                $this->set($this->ip + 3, $modes[2], $val);
                $this->ip += 4;
                break;
            case 2:
                $val = $this->get($this->ip + 1, $modes[0]) * $this->get($this->ip + 2, $modes[1]);
                $this->set($this->ip + 3, $modes[2], $val);
                $this->ip += 4;
                break;
            case 3:
                $this->set($this->ip + 1, $modes[0], $this->in->current());
                $this->in->next();
                $this->ip += 2;
                break;
            case 4:
                $this->out[] = $this->get($this->ip + 1, $modes[0]);
                $this->ip += 2;
                break;
            case 5:
                if ($this->get($this->ip + 1, $modes[0]) != 0) {
                    $this->ip = $this->get($this->ip + 2, $modes[1]);
                } else {
                    $this->ip += 3;
                }
                break;
            case 6:
                if ($this->get($this->ip + 1, $modes[0]) == 0) {
                    $this->ip = $this->get($this->ip + 2, $modes[1]);
                } else {
                    $this->ip += 3;
                }
                break;
            case 7:
                if ($this->get($this->ip + 1, $modes[0]) < $this->get($this->ip + 2, $modes[1])) {
                    $this->set($this->ip + 3, $modes[2], 1);
                } else {
                    $this->set($this->ip + 3, $modes[2], 0);
                }
                $this->ip += 4;
                break;
            case 8:
                if ($this->get($this->ip + 1, $modes[0]) == $this->get($this->ip + 2, $modes[1])) {
                    $this->set($this->ip + 3, $modes[2], 1);
                } else {
                    $this->set($this->ip + 3, $modes[2], 0);
                }
                $this->ip += 4;
                break;
            case 9:
                $this->relbase += $this->get($this->ip + 1, $modes[0]);
                $this->ip += 2;
                break;
            case 99:
                return false;
            default:
                throw new Exception("Unknown opcode: $op");
        }
        return true;
    }

    public function run(): array {
        while ($this->step()) {
        }
        return $this->out;
    }
}

function run(array $program, iterable $in): array {
    $out = [];
    $m = new Machine($program, new ArrayIterator($in), $out);
    return $m->run();
}

const N = 0;
const E = 1;
const S = 2;
const W = 3;

$point = [
    N => [0, 1],
    E => [1, 0],
    S => [0, -1],
    W => [-1, 0],
];

$pointReversed = [
    N => [0, -1],
    E => [1, 0],
    S => [0, 1],
    W => [-1, 0],
];

function point(int $d): array {
    global $point;
    return $point[$d];
}

function pointR(int $d): array {
    global $pointReversed;
    return $pointReversed[$d];
}

$fromPoint = [
    '0,1' => N,
    '1,0' => E,
    '0,-1' => S,
    '-1,0' => W,
];

function dirFromPoint(array $p): int {
    global $fromPoint;
    return $fromPoint["{$p[0]},{$p[1]}"];
}

function nextDir(int $d): int {
    return ($d + 1) % 4;
}

function prevDir(int $d): int {
    return ($d + 3) % 4;
}

$fromByte = [
    'N' => N,
    'E' => E,
    'S' => S,
    'W' => W,
    'U' => N,
    'R' => E,
    'D' => S,
    'L' => W,
    '^' => N,
    '>' => E,
    'v' => S,
    '<' => W,
];

function dirFromByte(string $b): int {
    global $fromByte;
    return $fromByte[$b];
}

function parse(array $program): array {
    $out = run($program, []);
    $sb = "";
    foreach ($out as $o) {
        $sb .= chr($o);
    }
    $scaffolding = [];
    $robot = [];
    $dir = 0;
    $lines = explode("\n", $sb);
    $y = 0;
    foreach ($lines as $line) {
        for ($x = 0; $x < strlen($line); $x++) {
            $char = $line[$x];
            if (in_array($char, ['^', 'v', '<', '>'])) {
                $robot = [$x, $y];
                $dir = dirFromByte($char);
            }
            if (in_array($char, ['#', '^', 'v', '<', '>'])) {
                $scaffolding["{$x},{$y}"] = true;
            }
        }
        $y++;
    }
    return [$scaffolding, $robot, $dir];
}

function addPoints(array $p1, array $p2): array {
    return [$p1[0] + $p2[0], $p1[1] + $p2[1]];
}

function path(array $scaffolding, array $robot, int $dir): string {
    $dist = 0;
    $d = '';
    $sections = [];
    while (true) {
        $nextPos = addPoints($robot, pointR($dir));
        if (isset($scaffolding["{$nextPos[0]},{$nextPos[1]}"])) {
            $robot = $nextPos;
            $dist++;
            continue;
        }
        if ($dist > 0) {
            $sections[] = "{$d},{$dist}";
        }
        $nextPos = addPoints($robot, pointR(nextDir($dir)));
        if (isset($scaffolding["{$nextPos[0]},{$nextPos[1]}"])) {
            $robot = $nextPos;
            $dir = nextDir($dir);
            $dist = 1;
            $d = 'R';
        } else {
            $nextPos = addPoints($robot, pointR(prevDir($dir)));
            if (isset($scaffolding["{$nextPos[0]},{$nextPos[1]}"])) {
                $robot = $nextPos;
                $dir = prevDir($dir);
                $dist = 1;
                $d = 'L';
            } else {
                break;
            }
        }
    }
    return implode(",", $sections);
}

function encode(string $path): array {
    for ($i = 2; $i <= 21; $i++) {
        for ($j = 2; $j <= 21; $j++) {
            for ($k = 2; $k <= 21; $k++) {
                $next = $path . ",";
                $a = substr($next, 0, $i);
                $next = str_replace($a, "", $next);
                $b = substr($next, 0, $j);
                $next = str_replace($b, "", $next);
                $c = substr($next, 0, $k);
                $next = str_replace($c, "", $next);
                if ($next == "") {
                    $a = trim($a, ",");
                    $b = trim($b, ",");
                    $c = trim($c, ",");
                    $path = str_replace($a, "A", $path);
                    $path = str_replace($b, "B", $path);
                    $path = str_replace($c, "C", $path);
                    $path = trim($path, ",");
                    return [$path, $a, $b, $c];
                }
            }
        }
    }
    return ["", "", "", ""];
}

function dust(array $program, array $scaffolding, array $robot, int $dir): int {
    [$seq, $a, $b, $c] = encode(path($scaffolding, $robot, $dir));
    $input = "{$seq}\n{$a}\n{$b}\n{$c}\nn\n";
    $in = [];
    for ($i = 0; $i < strlen($input); $i++) {
        $in[] = ord($input[$i]);
    }
    $program[0] = 2;
    $out = run($program, $in);
    return end($out);
}

$program = array_map('Atoi', explode(',', readAll("input.txt")));
[$scaffolding, $robot, $dir] = parse($program);
echo dust($program, $scaffolding, $robot, $dir) . PHP_EOL;
