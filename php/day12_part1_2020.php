<?php
class Ship {
    public $x, $y, $facing;

    public function processInstruction($action, $value) {
        switch ($action) {
            case 'N':
                $this->y += $value;
                break;
            case 'S':
                $this->y -= $value;
                break;
            case 'E':
                $this->x += $value;
                break;
            case 'W':
                $this->x -= $value;
                break;
            case 'L':
                $this->facing = ($this->facing - $value + 360) % 360;
                break;
            case 'R':
                $this->facing = ($this->facing + $value) % 360;
                break;
            case 'F':
                switch ($this->facing) {
                    case 0:
                        $this->x += $value;
                        break;
                    case 90:
                        $this->y -= $value;
                        break;
                    case 180:
                        $this->x -= $value;
                        break;
                    case 270:
                        $this->y += $value;
                        break;
                }
                break;
        }
    }
}

$file = fopen('input.txt', 'r');
$ship = new Ship();
$ship->x = $ship->y = $ship->facing = 0;

while (($line = fgets($file)) !== false) {
    $action = substr($line, 0, 1);
    $value = intval(substr($line, 1));
    $ship->processInstruction($action, $value);
}

fclose($file);

$manhattanDistance = abs($ship->x) + abs($ship->y);
echo $manhattanDistance;