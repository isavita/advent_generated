
<?php

class VM
{
    public array $code;
    public int $ip = 0;
    public int $relativeBase = 0;
    public array $input = [];
    public array $output = [];

    public function __construct(string $filename)
    {
        $this->load($filename);
    }

    public function load(string $filename): void
    {
        $content = trim(file_get_contents($filename));
        $this->code = array_map('intval', explode(',', $content));
    }

    public function run(): void
    {
        while (true) {
            $cmd = $this->code[$this->ip];
            $opCode = $cmd % 100;
            $modes = (int)($cmd / 100);
            $arity = 0;

            switch ($opCode) {
                case 1:
                    $arity = 3;
                    $params = $this->getParamsAddresses($this->ip, $modes, $arity);
                    $this->code[$params[2]] = $this->code[$params[0]] + $this->code[$params[1]];
                    break;
                case 2:
                    $arity = 3;
                    $params = $this->getParamsAddresses($this->ip, $modes, $arity);
                    $this->code[$params[2]] = $this->code[$params[0]] * $this->code[$params[1]];
                    break;
                case 3:
                    $arity = 1;
                    $params = $this->getParamsAddresses($this->ip, $modes, $arity);
                    $this->code[$params[0]] = array_shift($this->input);
                    break;
                case 4:
                    $arity = 1;
                    $params = $this->getParamsAddresses($this->ip, $modes, $arity);
                    $this->output[] = $this->code[$params[0]];
                    break;
                case 5:
                    $arity = 2;
                    $params = $this->getParamsAddresses($this->ip, $modes, $arity);
                    if ($this->code[$params[0]] != 0) {
                        $this->ip = $this->code[$params[1]];
                        continue 2;
                    }
                    break;
                case 6:
                    $arity = 2;
                    $params = $this->getParamsAddresses($this->ip, $modes, $arity);
                    if ($this->code[$params[0]] == 0) {
                        $this->ip = $this->code[$params[1]];
                        continue 2;
                    }
                    break;
                case 7:
                    $arity = 3;
                    $params = $this->getParamsAddresses($this->ip, $modes, $arity);
                    $this->code[$params[2]] = ($this->code[$params[0]] < $this->code[$params[1]]) ? 1 : 0;
                    break;
                case 8:
                    $arity = 3;
                    $params = $this->getParamsAddresses($this->ip, $modes, $arity);
                    $this->code[$params[2]] = ($this->code[$params[0]] == $this->code[$params[1]]) ? 1 : 0;
                    break;
                case 9:
                    $arity = 1;
                    $params = $this->getParamsAddresses($this->ip, $modes, $arity);
                    $this->relativeBase += $this->code[$params[0]];
                    break;
                case 99:
                    return;
                default:
                    throw new Exception("Invalid opcode: " . $opCode);
            }
            $this->ip += $arity + 1;
        }
    }

    private function getParamsAddresses(int $pos, int $modes, int $arity): array
    {
        $results = [];
        for ($i = 0; $i < $arity; $i++) {
            $mode = ($modes / (10 ** $i)) % 10;
            $results[] = $this->getParamAddress($pos + $i + 1, $mode);
        }
        return $results;
    }

    private function getParamAddress(int $pos, int $mode): int
    {
        return match ($mode) {
            0 => $this->code[$pos] ?? 0,
            1 => $pos,
            2 => ($this->relativeBase + ($this->code[$pos] ?? 0)),
            default => throw new Exception("Invalid mode: " . $mode),
        };
    }
}

function sendString(VM $vm, string $s): void
{
    foreach (str_split($s) as $char) {
        $vm->input[] = ord($char);
    }
    $vm->input[] = ord("\n");
}

$vm = new VM("input.txt");

$instructions = [
    "NOT A J",
    "NOT B T",
    "OR T J",
    "NOT C T",
    "OR T J",
    "AND D J",
    "NOT A T",
    "AND A T",
    "OR E T",
    "OR H T",
    "AND T J",
    "RUN",
];

foreach ($instructions as $instruction) {
    sendString($vm, $instruction);
}

$vm->run();

foreach ($vm->output as $c) {
    if ($c > 127) {
        echo $c . PHP_EOL;
        return;
    }
}
