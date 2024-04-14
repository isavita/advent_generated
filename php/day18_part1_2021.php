<?php

class SnailNumber {
    public $value;
    public $left;
    public $right;

    public function __construct($value = null, $left = null, $right = null) {
        $this->value = $value;
        $this->left = $left;
        $this->right = $right;
    }

    public function isRegular() {
        return $this->left === null && $this->right === null;
    }

    public function add($other) {
        $newNumber = new SnailNumber(null, $this, $other);
        return $newNumber->reduce();
    }

    public function reduce() {
        while (true) {
            list($exploded, $leftValue, $rightValue) = $this->explode(0);
            if ($exploded) {
                continue;
            }
            if (!$this->split()) {
                break;
            }
        }
        return $this;
    }

    public function explode($depth) {
        if ($this->isRegular()) {
            return [false, 0, 0];
        }

        if ($depth === 4) {
            $leftValue = $this->left->value;
            $rightValue = $this->right->value;
            $this->left = null;
            $this->right = null;
            $this->value = 0;
            return [true, $leftValue, $rightValue];
        }

        list($exploded, $leftValue, $rightValue) = $this->left->explode($depth + 1);
        if ($exploded) {
            if ($rightValue > 0 && $this->right !== null) {
                $this->right->addLeft($rightValue);
            }
            return [true, $leftValue, 0];
        }

        list($exploded, $leftValue, $rightValue) = $this->right->explode($depth + 1);
        if ($exploded) {
            if ($leftValue > 0 && $this->left !== null) {
                $this->left->addRight($leftValue);
            }
            return [true, 0, $rightValue];
        }

        return [false, 0, 0];
    }

    public function addLeft($value) {
        if ($this->isRegular()) {
            $this->value += $value;
        } else {
            $this->left->addLeft($value);
        }
    }

    public function addRight($value) {
        if ($this->isRegular()) {
            $this->value += $value;
        } else {
            $this->right->addRight($value);
        }
    }

    public function split() {
        if ($this->isRegular()) {
            if ($this->value >= 10) {
                $this->left = new SnailNumber(intval($this->value / 2));
                $this->right = new SnailNumber(intval(($this->value + 1) / 2));
                $this->value = null;
                return true;
            }
            return false;
        }
        return $this->left->split() || $this->right->split();
    }

    public function magnitude() {
        if ($this->isRegular()) {
            return $this->value;
        }
        return 3 * $this->left->magnitude() + 2 * $this->right->magnitude();
    }
}

function parseSnailNumber($input) {
    $input = trim($input);
    if ($input[0] !== '[') {
        return new SnailNumber(intval($input));
    }

    $balance = 0;
    $splitIndex = 0;
    $input = substr($input, 1, -1);

    for ($i = 0; $i < strlen($input); $i++) {
        $char = $input[$i];
        if ($char === '[') $balance++;
        if ($char === ']') $balance--;
        if ($char === ',' && $balance === 0) {
            $splitIndex = $i;
            break;
        }
    }

    $left = parseSnailNumber(substr($input, 0, $splitIndex));
    $right = parseSnailNumber(substr($input, $splitIndex + 1));
    return new SnailNumber(null, $left, $right);
}

$input = file_get_contents("input.txt");
$lines = explode("\n", trim($input));
$snailNumbers = array_map('parseSnailNumber', $lines);

$result = $snailNumbers[0];
for ($i = 1; $i < count($snailNumbers); $i++) {
    $result = $result->add($snailNumbers[$i]);
}

echo $result->magnitude();