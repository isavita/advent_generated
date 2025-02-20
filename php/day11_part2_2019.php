<?php

class IntcodeComputer {
    private $memory;
    private $ip = 0;
    private $relative_base = 0;
    private $halted = false;
    private $input_buffer = [];
    private $output_buffer = [];

    public function __construct($program) {
        $this->memory = $program;
    }

    private function getParamModes($instruction) {
        return [
            (int)(($instruction / 100) % 10),
            (int)(($instruction / 1000) % 10),
            (int)(($instruction / 10000) % 10),
        ];
    }

    private function getMemory($address) {
        return isset($this->memory[$address]) ? $this->memory[$address] : 0;
    }

    private function setMemory($address, $value) {
        $this->memory[$address] = $value;
    }

    public function addInput($value) {
        $this->input_buffer[] = $value;
    }

    public function getOutput() {
        return array_shift($this->output_buffer);
    }

    public function runStep() {
        if ($this->halted) return ['type' => 'halt'];

        $instruction = $this->getMemory($this->ip);
        $opcode = $instruction % 100;
        $modes = $this->getParamModes($instruction);

        switch ($opcode) {
            case 1: // ADD
                $val1 = $this->getParam($modes[0], $this->ip + 1);
                $val2 = $this->getParam($modes[1], $this->ip + 2);
                $this->setParam($modes[2], $this->ip + 3, $val1 + $val2);
                $this->ip += 4;
                break;
            case 2: // MUL
                $val1 = $this->getParam($modes[0], $this->ip + 1);
                $val2 = $this->getParam($modes[1], $this->ip + 2);
                $this->setParam($modes[2], $this->ip + 3, $val1 * $val2);
                $this->ip += 4;
                break;
            case 3: // INPUT
                if (empty($this->input_buffer)) return ['type' => 'input'];
                $input = array_shift($this->input_buffer);
                $this->setParam($modes[0], $this->ip + 1, $input);
                $this->ip += 2;
                break;
            case 4: // OUTPUT
                $output = $this->getParam($modes[0], $this->ip + 1);
                $this->output_buffer[] = $output;
                $this->ip += 2;
                return ['type' => 'output', 'value' => $output];
            case 5: // JMP-TRUE
                $val1 = $this->getParam($modes[0], $this->ip + 1);
                $val2 = $this->getParam($modes[1], $this->ip + 2);
                $this->ip = ($val1 != 0) ? $val2 : $this->ip + 3;
                break;
            case 6: // JMP-FALSE
                $val1 = $this->getParam($modes[0], $this->ip + 1);
                $val2 = $this->getParam($modes[1], $this->ip + 2);
                $this->ip = ($val1 == 0) ? $val2 : $this->ip + 3;
                break;
            case 7: // LESS-THAN
                $val1 = $this->getParam($modes[0], $this->ip + 1);
                $val2 = $this->getParam($modes[1], $this->ip + 2);
                $this->setParam($modes[2], $this->ip + 3, (int)($val1 < $val2));
                $this->ip += 4;
                break;
            case 8: // EQUALS
                $val1 = $this->getParam($modes[0], $this->ip + 1);
                $val2 = $this->getParam($modes[1], $this->ip + 2);
                $this->setParam($modes[2], $this->ip + 3, (int)($val1 == $val2));
                $this->ip += 4;
                break;
            case 9: // REL-BASE
                $this->relative_base += $this->getParam($modes[0], $this->ip + 1);
                $this->ip += 2;
                break;
            case 99: // HALT
                $this->halted = true;
                return ['type' => 'halt'];
            default:
                throw new Exception("Unknown opcode: $opcode");
        }
        return ['type' => 'continue'];
    }

    private function getParam($mode, $addr) {
        $param = $this->getMemory($addr);
        switch ($mode) {
            case 0: return $this->getMemory($param);
            case 1: return $param;
            case 2: return $this->getMemory($this->relative_base + $param);
            default: throw new Exception("Invalid parameter mode");
        }
    }

    private function setParam($mode, $addr, $value) {
        $param = $this->getMemory($addr);
        switch ($mode) {
            case 0: $this->setMemory($param, $value); break;
            case 2: $this->setMemory($this->relative_base + $param, $value); break;
            default: throw new Exception("Invalid parameter mode");
        }
    }
}

class Robot {
    private $computer;
    private $direction = 0;
    private $position = [0, 0];
    private $panels = [];
    private $painted = [];

    public function __construct($program, $startColor) {
        $this->computer = new IntcodeComputer($program);
        $this->panels[$this->posKey()] = $startColor;
    }

    private function posKey() {
        return "{$this->position[0]},{$this->position[1]}";
    }

    public function run() {
        $expectColor = true;
        while (true) {
            $result = $this->computer->runStep();
            
            if ($result['type'] === 'halt') break;
            
            if ($result['type'] === 'input') {
                $color = $this->panels[$this->posKey()] ?? 0;
                $this->computer->addInput($color);
            }
            
            if ($result['type'] === 'output') {
                if ($expectColor) {
                    $color = $result['value'];
                    $expectColor = false;
                } else {
                    $this->painted[$this->posKey()] = true;
                    $this->panels[$this->posKey()] = $color;
                    $this->turn($result['value']);
                    $this->move();
                    $expectColor = true;
                }
            }
        }
    }

    private function turn($direction) {
        $this->direction = ($direction === 0) 
            ? ($this->direction + 3) % 4 
            : ($this->direction + 1) % 4;
    }

    private function move() {
        switch ($this->direction) {
            case 0: $this->position[1]--; break; // Up
            case 1: $this->position[0]++; break; // Right
            case 2: $this->position[1]++; break; // Down
            case 3: $this->position[0]--; break; // Left
        }
    }

    public function getPaintedCount() {
        return count($this->painted);
    }

    public function render() {
        $minX = $maxX = $minY = $maxY = 0;
        foreach (array_keys($this->panels) as $key) {
            [$x, $y] = explode(',', $key);
            $minX = min($minX, $x);
            $maxX = max($maxX, $x);
            $minY = min($minY, $y);
            $maxY = max($maxY, $y);
        }

        echo "\n";
        for ($y = $minY; $y <= $maxY; $y++) {
            for ($x = $minX; $x <= $maxX; $x++) {
                $color = $this->panels["$x,$y"] ?? 0;
                echo $color ? '█' : ' ';
            }
            echo "\n";
        }
    }
}

function main() {
    $program = array_map('intval', explode(',', trim(file_get_contents('input.txt'))));
    
    $robot1 = new Robot($program, 0);
    $robot1->run();
    echo "Part 1: " . $robot1->getPaintedCount() . "\n";
    
    $robot2 = new Robot($program, 1);
    $robot2->run();
    echo "Part 2:\n";
    $robot2->render();
}

main();
