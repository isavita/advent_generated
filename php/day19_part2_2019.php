
<?php

class VM
{
    public array $code;
    public int $ip = 0;
    public int $relativeBase = 0;

    public function __construct(string $filename)
    {
        $this->load($filename);
    }

    private function load(string $filename): void
    {
        $content = file_get_contents($filename);
        $listStr = explode(',', trim($content));
        $this->code = array_map('intval', $listStr);
    }

    public function run(int $input1, int $input2): int
    {
        $this->ip = 0;
        $this->relativeBase = 0;
        $input = [$input1, $input2];
        $inputIndex = 0;
        $output = null;

        while (true) {
            $cmd = $this->code[$this->ip];
            $opCode = $cmd % 100;
            $modes = (int)($cmd / 100);

            switch ($opCode) {
                case 1:
                case 2:
                    $arity = 3;
                    $params = $this->getParamsAddresses($this->ip, $modes, $arity);
                    $this->code[$params[2]] = $opCode === 1 ? $this->code[$params[0]] + $this->code[$params[1]] : $this->code[$params[0]] * $this->code[$params[1]];
                    break;
                case 3:
                    $arity = 1;
                    $params = $this->getParamsAddresses($this->ip, $modes, $arity);
                    $this->code[$params[0]] = $input[$inputIndex++];
                    break;
                case 4:
                    $arity = 1;
                    $params = $this->getParamsAddresses($this->ip, $modes, $arity);
                    $output = $this->code[$params[0]];
                    break;
                case 5:
                case 6:
                    $arity = 2;
                    $params = $this->getParamsAddresses($this->ip, $modes, $arity);
                    if (($opCode === 5 && $this->code[$params[0]] != 0) || ($opCode === 6 && $this->code[$params[0]] == 0)) {
                        $this->ip = $this->code[$params[1]];
                        continue 2;
                    }
                    break;
                case 7:
                case 8:
                    $arity = 3;
                    $params = $this->getParamsAddresses($this->ip, $modes, $arity);
                    $this->code[$params[2]] = ($opCode === 7 ? $this->code[$params[0]] < $this->code[$params[1]] : $this->code[$params[0]] == $this->code[$params[1]]) ? 1 : 0;
                    break;
                case 9:
                    $arity = 1;
                    $params = $this->getParamsAddresses($this->ip, $modes, $arity);
                    $this->relativeBase += $this->code[$params[0]];
                    break;
                case 99:
                    return $output;
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
            $mode = $modes % 10;
            $modes = (int)($modes / 10);
            $results[] = $this->getParamAddress($pos + $i + 1, $mode);
        }
        return $results;
    }

    private function getParamAddress(int $pos, int $mode): int
    {
        $address = $this->code[$pos] ?? 0;
        switch ($mode) {
            case 0:
                return $address;
            case 1:
                return $pos;
            case 2:
                return $this->relativeBase + $address;
            default:
                throw new Exception("Invalid mode: " . $mode);
        }
    }
}

function beam(int $x, int $y): bool
{
    $vm = new VM("input.txt");
    return $vm->run($x, $y) === 1;
}

$y = 20;
$x = 0;

while (true) {
    if (!beam($x, $y)) {
        $x++;
        continue;
    }

    if (!beam($x + 99, $y)) {
        $y++;
        continue;
    }

    if (!beam($x, $y + 99)) {
        $x++;
        continue;
    }

    echo $x * 10000 + $y . PHP_EOL;
    break;
}
