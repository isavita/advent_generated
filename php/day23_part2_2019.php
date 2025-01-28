
<?php
class VM {
    public array $code;
    public int $ip = 0;
    public array $input = [];
    public array $output = [];
    public int $relativeBase = 0;

    public function __construct(string $filename) {
        $this->load($filename);
    }

    public function load(string $filename): void {
        $this->code = array_map('intval', explode(',', trim(file_get_contents($filename))));
    }

    public function run(): void {
        while (true) {
            $cmd = $this->code[$this->ip];
            $opcode = $cmd % 100;
            $arity = 0;

            switch ($opcode) {
                case 1: // add
                    $arity = 3;
                    $params = $this->getParamsAddresses($this->ip, $cmd, $arity);
                    $this->code[$params[2]] = ($this->code[$params[0]] ?? 0) + ($this->code[$params[1]] ?? 0);
                    break;
                case 2: // multiply
                    $arity = 3;
                    $params = $this->getParamsAddresses($this->ip, $cmd, $arity);
                    $this->code[$params[2]] = ($this->code[$params[0]] ?? 0) * ($this->code[$params[1]] ?? 0);
                    break;
                case 3: // read
                    $arity = 1;
                    $params = $this->getParamsAddresses($this->ip, $cmd, $arity);
                    if (empty($this->input)) {
                        return;
                    }
                    $this->code[$params[0]] = array_shift($this->input);
                    break;
                case 4: // write
                    $arity = 1;
                    $params = $this->getParamsAddresses($this->ip, $cmd, $arity);
                    $this->output[] = ($this->code[$params[0]] ?? 0);
                    break;
                case 5: // jump not zero
                    $arity = 2;
                    $params = $this->getParamsAddresses($this->ip, $cmd, $arity);
                    if (($this->code[$params[0]] ?? 0) != 0) {
                        $this->ip = ($this->code[$params[1]] ?? 0);
                        continue 2;
                    }
                    break;
                case 6: // jump zero
                    $arity = 2;
                    $params = $this->getParamsAddresses($this->ip, $cmd, $arity);
                    if (($this->code[$params[0]] ?? 0) == 0) {
                        $this->ip = ($this->code[$params[1]] ?? 0);
                        continue 2;
                    }
                    break;
                case 7: // less than
                    $arity = 3;
                    $params = $this->getParamsAddresses($this->ip, $cmd, $arity);
                    $this->code[$params[2]] = (($this->code[$params[0]] ?? 0) < ($this->code[$params[1]] ?? 0)) ? 1 : 0;
                    break;
                case 8: // equal
                    $arity = 3;
                    $params = $this->getParamsAddresses($this->ip, $cmd, $arity);
                    $this->code[$params[2]] = (($this->code[$params[0]] ?? 0) == ($this->code[$params[1]] ?? 0)) ? 1 : 0;
                    break;
                case 9: // change relative base
                    $arity = 1;
                    $params = $this->getParamsAddresses($this->ip, $cmd, $arity);
                    $this->relativeBase += ($this->code[$params[0]] ?? 0);
                    break;
                case 99: // halt
                    return;
                default:
                    exit("Error: Unknown opcode " . $opcode . " at position " . $this->ip);
            }
            $this->ip += $arity + 1;
        }
    }

    private function getParamsAddresses(int $pos, int $cmd, int $arity): array {
        $modes = intdiv($cmd, 100);
        $addresses = [];
        for ($i = 0; $i < $arity; $i++) {
            $mode = intdiv($modes, (10**$i)) % 10;
            switch ($mode) {
                case 0: // Position
                    $addresses[] = ($this->code[$pos + $i + 1] ?? 0);
                    break;
                case 1: // Immediate
                    $addresses[] = $pos + $i + 1;
                    break;
                case 2: // Relative
                    $addresses[] = $this->relativeBase + ($this->code[$pos + $i + 1] ?? 0);
                    break;
                default:
                    exit("Error: Unknown parameter mode " . $mode);
            }
        }
        return $addresses;
    }
}

const SIZE = 50;

$vms = [];
$natPackets = [];
$natY = -1;
$lastNatY = -1;
$idleCounter = 0;

for ($i = 0; $i < SIZE; $i++) {
    $vms[$i] = new VM("input.txt");
    $vms[$i]->input = [$i, -1];
}

while (true) {
    $idle = true;
    for ($i = 0; $i < SIZE; $i++) {
        $vm = $vms[$i];
        $input_len_before_run = count($vm->input);
        $vm->run();
        $output = $vm->output;
        $vm->output = [];

        if (count($vm->input) > $input_len_before_run) {
            $idle = false;
        }

        for ($j = 0; $j < count($output); $j += 3) {
            $address = $output[$j];
            $x = $output[$j+1];
            $y = $output[$j+2];

            if ($address === 255) {
                $natPackets = [$x, $y];
            } else if ($address >= 0 && $address < SIZE) {
                $vms[$address]->input[] = $x;
                $vms[$address]->input[] = $y;
                $idle = false;
            }
        }
    }

    if ($idle) {
        $idleCounter++;
    } else {
        $idleCounter = 0;
    }

    if ($idleCounter > 10 && !empty($natPackets)) {
        list($natX, $natY) = $natPackets;
        if ($natY === $lastNatY && $lastNatY !== -1) {
            echo $natY . "\n";
            exit;
        }
        $lastNatY = $natY;
        $vms[0]->input = [$natX, $natY];
        $idleCounter = 0;
    }
     usleep(10000); // Sleep for 10ms
}
?>
