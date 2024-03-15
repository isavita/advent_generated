<?php

const HASH_TABLE_SIZE = 256;

class Step {
    public $label;
    public $numBox;
    public $operation;
    public $number;
}

function hashString($str) {
    $res = 0;
    for ($i = 0; $i < strlen($str); $i++) {
        $char = ord($str[$i]);
        $res += $char;
        $res *= 17;
        $res %= HASH_TABLE_SIZE;
    }
    return $res;
}

function parseStep($stepStr) {
    $step = new Step();

    $step->label = rtrim($stepStr, "=-0123456789");
    $step->numBox = hashString($step->label);
    $step->operation = substr($stepStr, strlen($step->label), 1);
    if ($step->operation == "=") {
        $step->number = intval(substr($stepStr, strlen($step->label) + 1));
    }

    return $step;
}

function getBoxes($stepsStr) {
    $boxes = array_fill(0, HASH_TABLE_SIZE, []);

    foreach ($stepsStr as $stepStr) {
        $step = parseStep($stepStr);
        $boxContents = &$boxes[$step->numBox];

        switch ($step->operation) {
            case "-":
                for ($i = 0; $i < count($boxContents); $i++) {
                    if (array_key_exists($step->label, $boxContents[$i])) {
                        array_splice($boxContents, $i, 1);
                        break;
                    }
                }
                break;
            case "=":
                $found = false;
                for ($i = 0; $i < count($boxContents); $i++) {
                    if (array_key_exists($step->label, $boxContents[$i])) {
                        $boxContents[$i][$step->label] = $step->number;
                        $found = true;
                        break;
                    }
                }
                if (!$found) {
                    $boxContents[] = [$step->label => $step->number];
                }
                break;
        }

        if (count($boxContents) == 0) {
            $boxes[$step->numBox] = [];
        }
    }

    return $boxes;
}

function toStringBoxes($boxes) {
    $res = "";

    for ($iBox = 0; $iBox < HASH_TABLE_SIZE; $iBox++) {
        if (count($boxes[$iBox]) > 0) {
            $res .= "Box $iBox : ";
            foreach ($boxes[$iBox] as $content) {
                foreach ($content as $key => $value) {
                    $res .= "[$key $value] ";
                }
            }
            $res .= "\n";
        }
    }

    return $res;
}

function calculatePower($boxes) {
    $res = 0;

    for ($iBox = 0; $iBox < HASH_TABLE_SIZE; $iBox++) {
        if (count($boxes[$iBox]) > 0) {
            for ($iSlot = 0; $iSlot < count($boxes[$iBox]); $iSlot++) {
                foreach ($boxes[$iBox][$iSlot] as $value) {
                    $res += ($iBox + 1) * ($iSlot + 1) * $value;
                }
            }
        }
    }

    return $res;
}

function solve($input) {
    $stepsStr = explode(",", $input[0]);

    $boxes = getBoxes($stepsStr);

    return calculatePower($boxes);
}

$input = file("input.txt", FILE_IGNORE_NEW_LINES);
echo solve($input) . "\n";