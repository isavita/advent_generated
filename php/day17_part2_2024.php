
<?php
class Program {
    public int $a;
    public int $b;
    public int $c;
    public array $program;

    public function __construct(int $a, int $b, int $c, array $program) {
        $this->a = $a;
        $this->b = $b;
        $this->c = $c;
        $this->program = $program;
    }
}

class Pair {
    public int $a;
    public int $b;

    public function __construct(int $a, int $b) {
        $this->a = $a;
        $this->b = $b;
    }
}

function computeOperand(int $val, int $a, int $b, int $c): int {
    switch ($val) {
        case 0:
        case 1:
        case 2:
        case 3:
            return $val;
        case 4:
            return $a;
        case 5:
            return $b;
        case 6:
            return $c;
        default:
            throw new Exception("Invalid combo operand: " . $val);
    }
}

function simulateComputer(Program $program): array {
    $outs = [];
    $a = $program->a;
    $b = $program->b;
    $c = $program->c;
    $input = $program->program;

    for ($i = 0; $i < count($input); $i += 2) {
        $cmd = $input[$i];
        switch ($cmd) {
            case 0:
                $a >>= computeOperand($input[$i + 1], $a, $b, $c);
                break;
            case 1:
                $b ^= $input[$i + 1];
                break;
            case 2:
                $b = computeOperand($input[$i + 1], $a, $b, $c) % 8;
                break;
            case 3:
                if ($a != 0) {
                    $i = $input[$i + 1] - 2;
                }
                break;
            case 4:
                $b ^= $c;
                break;
            case 5:
                $outs[] = computeOperand($input[$i + 1], $a, $b, $c) % 8;
                break;
            case 6:
                $b = $a >> computeOperand($input[$i + 1], $a, $b, $c);
                break;
            case 7:
                $c = $a >> computeOperand($input[$i + 1], $a, $b, $c);
                break;
            default:
                throw new Exception("Invalid opcode: " . $cmd);
        }
    }
    return $outs;
}

function check(Program $p): array {
    $program = $p->program;
    $valids = [];
    $stack = [new Pair(0, 0)];
    $seen = [];

    while (!empty($stack)) {
        $state = array_pop($stack);

        $state_key = $state->a . "_" . $state->b;
        if (isset($seen[$state_key])) {
            continue;
        }
        $seen[$state_key] = true;

        $depth = $state->a;
        $score = $state->b;

        if ($depth == count($program)) {
            $valids[] = $score;
        } else {
            for ($i = 0; $i < 8; $i++) {
                $newScore = $i + 8 * $score;
                $testProgram = new Program($newScore, $p->b, $p->c, $program);
                $result = simulateComputer($testProgram);
                if (!empty($result) && $result[0] == $program[count($program) - 1 - $depth]) {
                    $stack[] = new Pair($depth + 1, $newScore);
                }
            }
        }
    }
    return $valids;
}


$file = fopen("input.txt", "r");
if (!$file) {
    throw new Exception("Failed to open input.txt");
}

$a = 0;
$b = 0;
$c = 0;
$program = [];

while (($line = fgets($file)) !== false) {
    $line = trim($line);
    if (strpos($line, "Register A:") === 0) {
        $parts = explode(":", $line);
        $a = intval(trim($parts[1]));
    } else if (strpos($line, "Register B:") === 0) {
        $parts = explode(":", $line);
        $b = intval(trim($parts[1]));
    } else if (strpos($line, "Register C:") === 0) {
        $parts = explode(":", $line);
        $c = intval(trim($parts[1]));
    } else if (strpos($line, "Program:") === 0) {
        $parts = explode(":", $line);
        $nums_str = trim($parts[1]);
        $nums = explode(",", $nums_str);
        foreach ($nums as $n) {
            $program[] = intval(trim($n));
        }
    }
}
fclose($file);

$p = new Program($a, $b, $c, $program);

$validValues = check($p);
$minVal = $validValues[0];
for ($i = 1; $i < count($validValues); $i++) {
    if ($validValues[$i] < $minVal) {
        $minVal = $validValues[$i];
    }
}

echo $minVal . "\n";
