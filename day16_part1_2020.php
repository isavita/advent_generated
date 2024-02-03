
<?php

class Rule {
    public $name;
    public $ranges;

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
    return (int)$s;
}

$filename = "input.txt";
$file = fopen($filename, "r") or die("Unable to open file!");

$rules = [];
$scanningRules = true;
$errorRate = 0;

$reRule = '/^([^:]+): (\d+)-(\d+) or (\d+)-(\d+)$/';

while (!feof($file)) {
    $line = trim(fgets($file));
    if ($line == "") {
        continue;
    }
    if (strpos($line, "your ticket:") !== false || strpos($line, "nearby tickets:") !== false) {
        $scanningRules = false;
        continue;
    }
    if ($scanningRules) {
        if (preg_match($reRule, $line, $matches)) {
            $name = $matches[1];
            $range1 = [toInt($matches[2]), toInt($matches[3])];
            $range2 = [toInt($matches[4]), toInt($matches[5])];
            $rules[] = new Rule($name, [$range1, $range2]);
        }
    } else {
        $values = explode(",", $line);
        foreach ($values as $value) {
            $val = toInt($value);
            if (!isValidForAnyRule($val, $rules)) {
                $errorRate += $val;
            }
        }
    }
}

echo $errorRate;

function isValidForAnyRule($value, $rules) {
    foreach ($rules as $rule) {
        if ($rule->isValid($value)) {
            return true;
        }
    }
    return false;
}

fclose($file);
?>
