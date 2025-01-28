
<?php

class IntcodeComputer {
    private array $memory;
    private int $instructionPointer = 0;
    private int $relativeBase = 0;
    private array $inputs = [];
    private array $outputs = [];

    public function __construct(array $program) {
        $this->memory = $program;
    }

    public function addInput(int $input): void {
        $this->inputs[] = $input;
    }

    public function getOutputs(): array {
        return $this->outputs;
    }
    
    public function clearOutputs(): void {
        $this->outputs = [];
    }

    public function run(): bool {
        while ($this->instructionPointer < count($this->memory)) {
            $opcode = $this->memory[$this->instructionPointer] % 100;
            $modes = str_pad((string)intdiv($this->memory[$this->instructionPointer], 100), 3, '0', STR_PAD_LEFT);

            switch ($opcode) {
                case 1:
                    $this->write(3, $modes, $this->read(1, $modes) + $this->read(2, $modes));
                    $this->instructionPointer += 4;
                    break;
                case 2:
                    $this->write(3, $modes, $this->read(1, $modes) * $this->read(2, $modes));
                    $this->instructionPointer += 4;
                    break;
                case 3:
                    if (empty($this->inputs)) {
                        return false;
                    }
                    $this->write(1, $modes, array_shift($this->inputs));
                    $this->instructionPointer += 2;
                    break;
                case 4:
                    $this->outputs[] = $this->read(1, $modes);
                    $this->instructionPointer += 2;
                    break;
                case 5:
                    if ($this->read(1, $modes) !== 0) {
                        $this->instructionPointer = $this->read(2, $modes);
                    } else {
                        $this->instructionPointer += 3;
                    }
                    break;
                case 6:
                    if ($this->read(1, $modes) === 0) {
                        $this->instructionPointer = $this->read(2, $modes);
                    } else {
                        $this->instructionPointer += 3;
                    }
                    break;
                case 7:
                    $this->write(3, $modes, $this->read(1, $modes) < $this->read(2, $modes) ? 1 : 0);
                    $this->instructionPointer += 4;
                    break;
                case 8:
                    $this->write(3, $modes, $this->read(1, $modes) === $this->read(2, $modes) ? 1 : 0);
                    $this->instructionPointer += 4;
                    break;
                case 9:
                    $this->relativeBase += $this->read(1, $modes);
                    $this->instructionPointer += 2;
                    break;
                case 99:
                    return true;
                default:
                    throw new RuntimeException("Unknown opcode: $opcode");
            }
        }
        return true;
    }

    private function read(int $offset, string $modes): int {
        $address = $this->getAddress($offset, $modes);
        return $this->memory[$address] ?? 0;
    }

    private function write(int $offset, string $modes, int $value): void {
        $address = $this->getAddress($offset, $modes);
        $this->memory[$address] = $value;
    }

    private function getAddress(int $offset, string $modes): int {
        $mode = $modes[3 - $offset];
        $value = $this->memory[$this->instructionPointer + $offset] ?? 0;

        return match ($mode) {
            '0' => $value,
            '1' => $this->instructionPointer + $offset,
            '2' => $this->relativeBase + $value,
            default => throw new RuntimeException("Unknown mode: $mode")
        };
    }
}

$program = explode(',', trim(file_get_contents('input.txt')));

$computers = [];
for ($i = 0; $i < 50; $i++) {
    $computers[$i] = new IntcodeComputer($program);
    $computers[$i]->addInput($i);
}

$queues = array_fill(0, 50, []);

while (true) {
    for ($i = 0; $i < 50; $i++) {
        if (empty($queues[$i])) {
            $computers[$i]->addInput(-1);
        } else {
            $packet = array_shift($queues[$i]);
            $computers[$i]->addInput($packet[0]);
            $computers[$i]->addInput($packet[1]);
        }

        $computers[$i]->run();
        $outputs = $computers[$i]->getOutputs();
        $computers[$i]->clearOutputs();
        
        for ($j = 0; $j < count($outputs); $j += 3) {
            $address = $outputs[$j];
            $x = $outputs[$j + 1];
            $y = $outputs[$j + 2];

            if ($address === 255) {
                echo $y . PHP_EOL;
                exit;
            }

            $queues[$address][] = [$x, $y];
        }
    }
}

?>
