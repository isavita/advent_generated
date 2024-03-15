<?php

class PanelColor
{
    const Black = 0;
    const White = 1;
}

class Direction
{
    const Up = 0;
    const Right = 1;
    const Down = 2;
    const Left = 3;
}

class Position
{
    public $x, $y;

    public function __construct($x, $y)
    {
        $this->x = $x;
        $this->y = $y;
    }
}

class Robot
{
    public $position;
    public $direction;

    public function __construct($position, $direction)
    {
        $this->position = $position;
        $this->direction = $direction;
    }

    public function turnAndMove($turnDirection)
    {
        if ($turnDirection == 0) {
            $this->direction = ($this->direction + 3) % 4; // Turn left
        } else {
            $this->direction = ($this->direction + 1) % 4; // Turn right
        }

        switch ($this->direction) {
            case Direction::Up:
                $this->position->y--;
                break;
            case Direction::Right:
                $this->position->x++;
                break;
            case Direction::Down:
                $this->position->y++;
                break;
            case Direction::Left:
                $this->position->x--;
                break;
        }
    }
}

class Intcode
{
    private $memory;
    private $ip;
    private $input;
    private $output;
    private $halted;

    public function __construct($program)
    {
        $this->memory = $program;
        $this->ip = 0;
        $this->input = [];
        $this->output = [];
        $this->halted = false;
    }

    public function addInput($input)
    {
        $this->input[] = $input;
    }

    public function run()
    {
        $this->output = [];
        while (true) {
            $opcode = $this->memory[$this->ip] % 100;
            switch ($opcode) {
                case 1: // Add
                case 2: // Multiply
                case 7: // Less than
                case 8: // Equals
                    $params = $this->getParams(3);
                    $val1 = $this->readMemory($params[0]);
                    $val2 = $this->readMemory($params[1]);
                    if ($opcode == 1) {
                        $this->writeMemory($params[2], $val1 + $val2);
                    } elseif ($opcode == 2) {
                        $this->writeMemory($params[2], $val1 * $val2);
                    } elseif ($opcode == 7 && $val1 < $val2 || $opcode == 8 && $val1 == $val2) {
                        $this->writeMemory($params[2], 1);
                    } else {
                        $this->writeMemory($params[2], 0);
                    }
                    $this->ip += 4;
                    break;
                case 3: // Input
                    $params = $this->getParams(1);
                    if (empty($this->input)) {
                        return; // Wait for more input
                    }
                    $this->writeMemory($params[0], array_shift($this->input));
                    $this->ip += 2;
                    break;
                case 4: // Output
                    $params = $this->getParams(1);
                    $this->output[] = $this->readMemory($params[0]);
                    $this->ip += 2;
                    break;
                case 5: // Jump-if-true
                case 6: // Jump-if-false
                    $params = $this->getParams(2);
                    $val = $this->readMemory($params[0]);
                    $target = $this->readMemory($params[1]);
                    if (($opcode == 5 && $val != 0) || ($opcode == 6 && $val == 0)) {
                        $this->ip = $target;
                    } else {
                        $this->ip += 3;
                    }
                    break;
                case 99: // Halt
                    $this->halted = true;
                    return;
                default:
                    throw new Exception("Unknown opcode: $opcode");
            }
        }
    }

    private function readMemory($address)
    {
        $this->ensureMemory($address);
        return $this->memory[$address];
    }

    private function writeMemory($address, $value)
    {
        $this->ensureMemory($address);
        $this->memory[$address] = $value;
    }

    private function ensureMemory($address)
    {
        if ($address >= count($this->memory)) {
            $this->memory = array_pad($this->memory, $address + 1, 0);
        }
    }

    private function getParams($count)
    {
        $paramModes = (int)($this->memory[$this->ip] / 100);
        $params = [];
        for ($i = 0; $i < $count; $i++) {
            if ($paramModes % 10 == 1) {
                $params[$i] = $this->ip + $i + 1;
            } else {
                $params[$i] = $this->memory[$this->ip + $i + 1];
            }
            $paramModes = (int)($paramModes / 10);
        }
        return $params;
    }

    public function getOutput()
    {
        return $this->output;
    }

    public function isHalted()
    {
        return $this->halted;
    }
}

$data = file_get_contents('input.txt');
$codeStr = explode(',', trim($data));
$program = array_map('intval', $codeStr);

$grid = [];
$robot = new Robot(new Position(0, 0), Direction::Up);
$intcode = new Intcode($program);

while (!$intcode->isHalted()) {
    $currentColor = isset($grid[$robot->position->x . ',' . $robot->position->y]) ? $grid[$robot->position->x . ',' . $robot->position->y] : PanelColor::Black;
    $intcode->addInput($currentColor);
    $intcode->run();
    $outputs = $intcode->getOutput();

    if (count($outputs) == 2) {
        $grid[$robot->position->x . ',' . $robot->position->y] = $outputs[0];
        $robot->turnAndMove($outputs[1]);
    }
}

echo count($grid) . "\n";