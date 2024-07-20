
<?php

class Module {
    public $name;
    public $prefix;
    public $destinations = [];
    public $state = false;
    public $memory = [];

    public function __construct($name, $prefix, $destinations) {
        $this->name = $name;
        $this->prefix = $prefix;
        $this->destinations = $destinations;
    }
}

class Pulse {
    public $value;
    public $fromName;
    public $toName;

    public function __construct($value, $fromName, $toName) {
        $this->value = $value;
        $this->fromName = $fromName;
        $this->toName = $toName;
    }
}

const LOW = 0;
const HIGH = 1;
const FLIP_FLOP = '%';
const CONJUNCTION = '&';

function parseInput($input) {
    $modules = [];
    foreach ($input as $line) {
        [$part, $destinations] = explode(' -> ', $line);
        $prefix = '';
        if (strpos($part, FLIP_FLOP) === 0) {
            $prefix = FLIP_FLOP;
            $name = substr($part, 1);
        } elseif (strpos($part, CONJUNCTION) === 0) {
            $prefix = CONJUNCTION;
            $name = substr($part, 1);
        } else {
            $name = $part;
        }
        $modules[$name] = new Module($name, $prefix, explode(', ', $destinations));
    }

    foreach ($modules as $module) {
        foreach ($module->destinations as $destName) {
            if (isset($modules[$destName]) && $modules[$destName]->prefix === CONJUNCTION) {
                $modules[$destName]->memory[$module->name] = LOW;
            }
        }
    }

    return $modules;
}

function pushButton($modules, $startPulse, $numCycle) {
    $cntLow = $cntHigh = 0;
    $pulseQueue = [];

    for ($i = 0; $i < $numCycle; $i++) {
        $pulseQueue[] = $startPulse;

        while ($pulseQueue) {
            $pulse = array_shift($pulseQueue);
            $pulseValue = $pulse->value;
            if ($pulseValue === LOW) $cntLow++;
            else $cntHigh++;

            if (!isset($modules[$pulse->toName])) continue;

            $module = $modules[$pulse->toName];
            $newPulseValue = $pulseValue;

            switch ($module->prefix) {
                case FLIP_FLOP:
                    if ($pulseValue === LOW) {
                        $module->state = !$module->state;
                        $newPulseValue = $module->state ? HIGH : LOW;
                    } else {
                        continue 2;
                    }
                    break;

                case CONJUNCTION:
                    $module->memory[$pulse->fromName] = $pulseValue;
                    $newPulseValue = (count(array_filter($module->memory)) === count($module->memory)) ? LOW : HIGH;
                    break;
            }

            foreach ($module->destinations as $destName) {
                $pulseQueue[] = new Pulse($newPulseValue, $pulse->toName, $destName);
            }
        }
    }

    return [$cntLow, $cntHigh];
}

function solve($input) {
    $startPulse = new Pulse(LOW, "button", "broadcaster");
    $numCycle = 1000;
    $modules = parseInput($input);
    [$cntLow, $cntHigh] = pushButton($modules, $startPulse, $numCycle);
    return $cntLow * $cntHigh;
}

function readFileContents($fileName) {
    return array_filter(array_map('trim', file($fileName)));
}

$input = readFileContents("input.txt");
echo solve($input);
