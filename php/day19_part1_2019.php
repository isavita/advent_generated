
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
        $content = file_get_contents($filename);
        $listStr = explode(',', trim($content));
        $this->code = array_map('intval', $listStr);
    }

    public function run(): void
    {
        while (true) {
            $cmd = $this->code[$this->ip];
            $opCode = $cmd % 100;
            $modes = $this->getModes($cmd, $opCode);

            switch ($opCode) {
                case 1:
                    $params = $this->getParamsAddresses($this->ip, $modes, 3);
                    $this->code[$params[2]] = $this->code[$params[0]] + $this->code[$params[1]];
                    $this->ip += 4;
                    break;
                case 2:
                    $params = $this->getParamsAddresses($this->ip, $modes, 3);
                    $this->code[$params[2]] = $this->code[$params[0]] * $this->code[$params[1]];
                    $this->ip += 4;
                    break;
                case 3:
                    $params = $this->getParamsAddresses($this->ip, $modes, 1);
                    $this->code[$params[0]] = array_shift($this->input);
                    $this->ip += 2;
                    break;
                case 4:
                    $params = $this->getParamsAddresses($this->ip, $modes, 1);
                    $this->output[] = $this->code[$params[0]];
                    $this->ip += 2;
                    break;
                case 5:
                    $params = $this->getParamsAddresses($this->ip, $modes, 2);
                    if ($this->code[$params[0]] != 0) {
                        $this->ip = $this->code[$params[1]];
                    } else {
                        $this->ip += 3;
                    }
                    break;
                case 6:
                    $params = $this->getParamsAddresses($this->ip, $modes, 2);
                    if ($this->code[$params[0]] == 0) {
                        $this->ip = $this->code[$params[1]];
                    } else {
                        $this->ip += 3;
                    }
                    break;
                case 7:
                    $params = $this->getParamsAddresses($this->ip, $modes, 3);
                    $this->code[$params[2]] = $this->code[$params[0]] < $this->code[$params[1]] ? 1 : 0;
                    $this->ip += 4;
                    break;
                case 8:
                    $params = $this->getParamsAddresses($this->ip, $modes, 3);
                    $this->code[$params[2]] = $this->code[$params[0]] == $this->code[$params[1]] ? 1 : 0;
                    $this->ip += 4;
                    break;
                case 9:
                    $params = $this->getParamsAddresses($this->ip, $modes, 1);
                    $this->relativeBase += $this->code[$params[0]];
                    $this->ip += 2;
                    break;
                case 99:
                    return;
                default:
                    throw new Exception("Invalid opcode: " . $opCode);
            }
        }
    }

    private function getModes(int $cmd, int $opCode): array
    {
        $modes = [];
        $cmd = (int) ($cmd / 100);
        $arity = match ($opCode) {
            1, 2, 7, 8 => 3,
            5, 6 => 2,
            3, 4, 9 => 1,
            default => 0,
        };
        for ($i = 0; $i < $arity; $i++) {
            $modes[] = ($cmd % 10);
            $cmd = (int) ($cmd / 10);
        }
        return $modes;
    }

    private function getParamsAddresses(int $pos, array $modes, int $arity): array
    {
        $addresses = [];
        for ($i = 0; $i < $arity; $i++) {
            $addresses[] = $this->getParamAddress($pos + $i + 1, $modes[$i]);
        }
        return $addresses;
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

function beam(int $x, int $y): bool
{
    $vm = new VM("input.txt");
    $vm->input = [$x, $y];
    $vm->run();
    return $vm->output[0] == 1;
}

$sum = 0;
for ($y = 0; $y < 50; $y++) {
    for ($x = 0; $x < 50; $x++) {
        if (beam($x, $y)) {
            $sum++;
        }
    }
}

echo $sum . PHP_EOL;
