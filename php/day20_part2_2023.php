
<?php

class Module {
    public $name;
    public $moduleType;
    public $connectsTo;
    public $state;
    public $watches;

    public function __construct($name, $moduleType, $connectsTo) {
        $this->name = $name;
        $this->moduleType = $moduleType;
        $this->connectsTo = $connectsTo;
        $this->state = false;
        $this->watches = [];
    }
}

const BROADCASTER = 0;
const FLIP_FLOP = 1;
const CONJUNCTION = 2;

function handleLine($line, &$connections) {
    if (strpos($line, 'broadcaster') !== false) {
        list($name, $connectsTo) = explode(' -> ', $line);
        $connections[$name] = new Module($name, BROADCASTER, explode(', ', $connectsTo));
    } elseif (strpos($line, '%') !== false) {
        list($name, $connectsTo) = explode(' -> ', $line);
        $connections[substr($name, 1)] = new Module(substr($name, 1), FLIP_FLOP, explode(', ', $connectsTo));
    } else {
        list($name, $connectsTo) = explode(' -> ', $line);
        $connections[substr($name, 1)] = new Module(substr($name, 1), CONJUNCTION, explode(', ', $connectsTo));
    }
}

function completeWatches(&$connections) {
    foreach ($connections as $module) {
        if ($module->moduleType === CONJUNCTION) {
            foreach ($connections as $module2) {
                if ($module2->moduleType === FLIP_FLOP || $module2->moduleType === CONJUNCTION) {
                    foreach ($module2->connectsTo as $name) {
                        if ($name === $module->name) {
                            $module->watches[$module2->name] = false;
                        }
                    }
                }
            }
        }
    }
}

function simulatePress(&$connections, &$loops, $pressNumber) {
    $queue = [['button', 'broadcaster', false]];
    $pulses = [1, 0];
    $found = false;

    while ($queue) {
        [$from, $name, $pulse] = array_shift($queue);
        $module = $connections[$name];

        if ($name === 'out') continue;
        if ($name === 'rx' && !$pulse) $found = true;

        switch ($module->moduleType) {
            case BROADCASTER:
                foreach ($module->connectsTo as $next) {
                    $queue[] = [$module->name, $next, $pulse];
                    $pulse ? $pulses[1]++ : $pulses[0]++;
                }
                break;
            case FLIP_FLOP:
                if (!$pulse) {
                    $module->state = !$module->state;
                    foreach ($module->connectsTo as $next) {
                        $queue[] = [$module->name, $next, $module->state];
                        $module->state ? $pulses[1]++ : $pulses[0]++;
                    }
                }
                break;
            case CONJUNCTION:
                $module->watches[$from] = $pulse;
                $allTrue = !in_array(false, $module->watches);
                foreach ($module->connectsTo as $next) {
                    $queue[] = [$module->name, $next, !$allTrue];
                    !$allTrue ? $pulses[1]++ : $pulses[0]++;
                }
                if (isset($loops[$module->name]) && !$allTrue && $loops[$module->name] === -1) {
                    $loops[$module->name] = $pressNumber;
                }
                break;
        }
    }
    return [$pulses, $found];
}

function sumHistory($hist) {
    $sum = [0, 0];
    foreach ($hist as $pulses) {
        $sum[0] += $pulses[0];
        $sum[1] += $pulses[1];
    }
    return $sum[0] * $sum[1];
}

function connectsTo($from, $to, $connections) {
    $module = $connections[$from];
    return in_array($to, $module->connectsTo);
}

$connections = [];
$input = file('input.txt', FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
foreach ($input as $line) {
    handleLine($line, $connections);
}

completeWatches($connections);

$pxPrev = array_filter(array_keys($connections), fn($k) => connectsTo($k, 'rx', $connections));
if (count($pxPrev) !== 1) throw new Exception("Error: more than one pxPrev");

$conj = $connections[array_shift($pxPrev)];
$loopLengths = array_fill_keys(array_keys($conj->watches), -1);

$pressNumber = 0;
while (true) {
    $pressNumber++;
    [$pulses, $found] = simulatePress($connections, $loopLengths, $pressNumber);
    if ($found) break;
    if (count(array_filter($loopLengths, fn($l) => $l === -1)) === 0) break;
}

$sum = array_product(array_values($loopLengths));
echo "$sum\n";
