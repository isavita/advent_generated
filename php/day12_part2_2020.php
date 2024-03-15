<?php
class Ship {
    public $x, $y, $waypointX, $waypointY;

    public function __construct() {
        $this->x = $this->y = 0;
        $this->waypointX = 10;
        $this->waypointY = 1;
    }

    public function processInstruction($action, $value) {
        switch ($action) {
            case 'N':
                $this->waypointY += $value;
                break;
            case 'S':
                $this->waypointY -= $value;
                break;
            case 'E':
                $this->waypointX += $value;
                break;
            case 'W':
                $this->waypointX -= $value;
                break;
            case 'L':
                $this->rotateWaypoint(-$value);
                break;
            case 'R':
                $this->rotateWaypoint($value);
                break;
            case 'F':
                $this->x += $this->waypointX * $value;
                $this->y += $this->waypointY * $value;
                break;
        }
    }

    private function rotateWaypoint($degrees) {
        $degrees = ($degrees + 360) % 360;
        switch ($degrees) {
            case 90:
            case -270:
                $temp = $this->waypointX;
                $this->waypointX = $this->waypointY;
                $this->waypointY = -$temp;
                break;
            case 180:
            case -180:
                $this->waypointX = -$this->waypointX;
                $this->waypointY = -$this->waypointY;
                break;
            case 270:
            case -90:
                $temp = $this->waypointX;
                $this->waypointX = -$this->waypointY;
                $this->waypointY = $temp;
                break;
        }
    }

    public function getManhattanDistance() {
        return abs($this->x) + abs($this->y);
    }
}

$file = fopen("input.txt", "r");
$ship = new Ship();
while (($line = fgets($file)) !== false) {
    $action = substr($line, 0, 1);
    $value = intval(substr($line, 1));
    $ship->processInstruction($action, $value);
}
fclose($file);

$manhattanDistance = $ship->getManhattanDistance();
echo $manhattanDistance;
?>