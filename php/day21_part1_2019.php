
<?php

class VM
{
    public array $code;
    public int $ip = 0;
    public array $input = [];
    public array $output = [];
    public int $relativeBase = 0;

    public function __construct(string $filename)
    {
        $this->load($filename);
    }

    public function load(string $filename): void
    {
        $content = file_get_contents($filename);
        $listStr = explode(',', trim($content));
        $this->code = array_map('intval', $listStr);
    }

    public function run(): void
    {
        while (true) {
            $cmd = $this->code[$this->ip];
            $opCode = $cmd % 100;
            $arity = 0;

            switch ($opCode) {
                case 1:
                    $arity = 3;
                    $params = $this->getParamsAddresses($this->ip, $cmd, $arity);
                    $this->code[$params[2]] = $this->code[$params[0]] + $this->code[$params[1]];
                    break;
                case 2:
                    $arity = 3;
                    $params = $this->getParamsAddresses($this->ip, $cmd, $arity);
                    $this->code[$params[2]] = $this->code[$params[0]] * $this->code[$params[1]];
                    break;
                case 3:
                    $arity = 1;
                    $params = $this->getParamsAddresses($this->ip, $cmd, $arity);
                    $this->code[$params[0]] = array_shift($this->input);
                    break;
                case 4:
                    $arity = 1;
                    $params = $this->getParamsAddresses($this->ip, $cmd, $arity);
                    $this->output[] = $this->code[$params[0]];
                    break;
                case 5:
                    $arity = 2;
                    $params = $this->getParamsAddresses($this->ip, $cmd, $arity);
                    if ($this->code[$params[0]] != 0) {
                        $this->ip = $this->code[$params[1]];
                        continue 2;
                    }
                    break;
                case 6:
                    $arity = 2;
                    $params = $this->getParamsAddresses($this->ip, $cmd, $arity);
                    if ($this->code[$params[0]] == 0) {
                        $this->ip = $this->code[$params[1]];
                        continue 2;
                    }
                    break;
                case 7:
                    $arity = 3;
                    $params = $this->getParamsAddresses($this->ip, $cmd, $arity);
                    $this->code[$params[2]] = ($this->code[$params[0]] < $this->code[$params[1]]) ? 1 : 0;
                    break;
                case 8:
                    $arity = 3;
                    $params = $this->getParamsAddresses($this->ip, $cmd, $arity);
                    $this->code[$params[2]] = ($this->code[$params[0]] == $this->code[$params[1]]) ? 1 : 0;
                    break;
                case 9:
                    $arity = 1;
                    $params = $this->getParamsAddresses($this->ip, $cmd, $arity);
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

    private function getParamsAddresses(int $pos, int $cmd, int $arity): array
    {
        $modes = $this->getModes($cmd, $arity);
        $results = [];
        for ($i = 0; $i < $arity; $i++) {
            $results[] = $this->getParamAddress($pos + $i + 1, $modes[$i]);
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

    private function getModes(int $cmd, int $arity): array
    {
        $modeSection = (int)($cmd / 100);
        $modes = [];
        for ($i = 0; $i < $arity; $i++) {
            $modes[] = (int)($modeSection / (10 ** $i) % 10);
        }
        return $modes;
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
    "WALK",
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
