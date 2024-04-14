<?php
class Rule {
    public $name;
    public $ranges = [];

    public function __construct($name, $ranges) {
        $this->name = $name;
        $this->ranges = $ranges;
    }

    public function isValid($value) {
        foreach ($this->ranges as $range) {
            if ($value >= $range[0] && $value <= $range[1]) {
                return true;
            }
        }
        return false;
    }
}

function toInt($s) {
    return intval($s);
}

function parseTicket($s) {
    return array_map('toInt', explode(',', $s));
}

function isValidTicket($ticket, $rules) {
    foreach ($ticket as $value) {
        if (!isValidForAnyRule($value, $rules)) {
            return false;
        }
    }
    return true;
}

function isValidForAnyRule($value, $rules) {
    foreach ($rules as $rule) {
        if ($rule->isValid($value)) {
            return true;
        }
    }
    return false;
}

function solveFieldPositions($rules, $tickets) {
    $validPositions = [];
    foreach ($rules as $rule) {
        $validPositions[$rule->name] = [];
        for ($i = 0; $i < count($tickets[0]); $i++) {
            $valid = true;
            foreach ($tickets as $ticket) {
                if (!$rule->isValid($ticket[$i])) {
                    $valid = false;
                    break;
                }
            }
            if ($valid) {
                $validPositions[$rule->name][$i] = true;
            }
        }
    }

    $fieldPositions = [];
    while (count($fieldPositions) < count($rules)) {
        foreach ($validPositions as $name => $positions) {
            if (count($positions) == 1) {
                $pos = array_keys($positions)[0];
                $fieldPositions[$name] = $pos;
                foreach ($validPositions as $otherName => &$otherPositions) {
                    unset($otherPositions[$pos]);
                }
                unset($validPositions[$name]);
            }
        }
    }
    return $fieldPositions;
}

function calculateDepartureProduct($ticket, $fieldPositions) {
    $product = 1;
    foreach ($fieldPositions as $name => $pos) {
        if (strpos($name, "departure") === 0) {
            $product *= $ticket[$pos];
        }
    }
    return $product;
}

$file = fopen("input.txt", "r");
if (!$file) {
    echo "File reading error\n";
    return;
}

$rules = [];
$myTicket = [];
$nearbyTickets = [];
$section = 0;
$reRule = '/^([^:]+): (\d+)-(\d+) or (\d+)-(\d+)$/';

while (($line = fgets($file)) !== false) {
    $line = trim($line);
    if ($line == "") {
        $section++;
        continue;
    }
    switch ($section) {
        case 0:
            if (preg_match($reRule, $line, $parts)) {
                $rule = new Rule($parts[1], [[toInt($parts[2]), toInt($parts[3])], [toInt($parts[4]), toInt($parts[5])]]);
                $rules[] = $rule;
            }
            break;
        case 1:
            if ($line != "your ticket:") {
                $myTicket = parseTicket($line);
            }
            break;
        case 2:
            if ($line != "nearby tickets:") {
                $ticket = parseTicket($line);
                if (isValidTicket($ticket, $rules)) {
                    $nearbyTickets[] = $ticket;
                }
            }
            break;
    }
}
fclose($file);

$fieldPositions = solveFieldPositions($rules, $nearbyTickets);
$departureProduct = calculateDepartureProduct($myTicket, $fieldPositions);

echo $departureProduct . "\n";
?>