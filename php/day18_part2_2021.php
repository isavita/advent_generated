<?php

class SnailNumber {
    public $value;
    public $left;
    public $right;

    public function __construct($value = 0, $left = null, $right = null) {
        $this->value = $value;
        $this->left = $left;
        $this->right = $right;
    }

    public function isRegular() {
        return $this->left === null && $this->right === null;
    }

    public function add($other) {
        $newNumber = new SnailNumber(0, $this, $other);
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
                $this->left = new SnailNumber(intdiv($this->value, 2));
                $this->right = new SnailNumber(intdiv($this->value + 1, 2));
                $this->value = -1;
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

    public function deepCopy() {
        if ($this->isRegular()) {
            return new SnailNumber($this->value);
        }
        return new SnailNumber(0, $this->left->deepCopy(), $this->right->deepCopy());
    }
}

function parseSnailNumber($input) {
    $input = trim($input);
    if ($input[0] !== '[') {
        return new SnailNumber(intval($input));
    }

    $balance = 0;
    $splitIndex = 0;
    for ($i = 1; $i < strlen($input) - 1; $i++) {
        $char = $input[$i];
        switch ($char) {
            case '[':
                $balance++;
                break;
            case ']':
                $balance--;
                break;
            case ',':
                if ($balance === 0) {
                    $splitIndex = $i;
                    break 2;
                }
                break;
        }
    }

    $left = parseSnailNumber(substr($input, 1, $splitIndex - 1));
    $right = parseSnailNumber(substr($input, $splitIndex + 1, -1));
    return new SnailNumber(0, $left, $right);
}

$lines = file('input.txt', FILE_IGNORE_NEW_LINES);
$snailNumbers = array_map('parseSnailNumber', $lines);

$largestMagnitude = 0;
foreach ($snailNumbers as $i => $a) {
    foreach ($snailNumbers as $j => $b) {
        if ($i === $j) {
            continue;
        }
        $sum1 = $a->deepCopy()->add($b->deepCopy())->magnitude();
        $sum2 = $b->deepCopy()->add($a->deepCopy())->magnitude();
        $largestMagnitude = max($largestMagnitude, $sum1, $sum2);
    }
}

echo $largestMagnitude;