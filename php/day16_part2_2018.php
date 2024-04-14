<?php

class OP {
    public $a;
    public $b;
    public $action;
    public $name;
    public $matchCount = [];

    public function __construct($name, $action, $a, $b) {
        $this->name = $name;
        $this->action = $action;
        $this->a = $a;
        $this->b = $b;
    }
}

function regSplit($text, $delimiter) {
    preg_match_all('/' . $delimiter . '/', $text, $matches, PREG_OFFSET_CAPTURE);
    $lastStart = 0;
    $result = [];
    foreach ($matches[0] as $match) {
        $result[] = substr($text, $lastStart, $match[1] - $lastStart);
        $lastStart = $match[1] + strlen($match[0]);
    }
    $result[] = substr($text, $lastStart);
    return $result;
}

function strToInt($s) {
    return intval($s);
}

function runOp($op, $registers, $instruction) {
    $registerCP = $registers;
    $A = $op->a === 'r' ? $registerCP[$instruction[1]] : $instruction[1];
    $B = $op->b === 'r' ? $registerCP[$instruction[2]] : $instruction[2];
    switch ($op->action) {
        case '+':
            $registerCP[$instruction[3]] = $A + $B;
            break;
        case '*':
            $registerCP[$instruction[3]] = $A * $B;
            break;
        case '&':
            $registerCP[$instruction[3]] = $A & $B;
            break;
        case '|':
            $registerCP[$instruction[3]] = $A | $B;
            break;
        case 'a':
            $registerCP[$instruction[3]] = $A;
            break;
        case '>':
            $registerCP[$instruction[3]] = $A > $B ? 1 : 0;
            break;
        case '=':
            $registerCP[$instruction[3]] = $A == $B ? 1 : 0;
            break;
    }
    return $registerCP;
}

function compareArrays($r, $c) {
    if (count($r) != count($c)) {
        return false;
    }
    foreach ($r as $i => $value) {
        if ($value != $c[$i]) {
            return false;
        }
    }
    return true;
}

function add(&$op, $c) {
    if (!in_array($c, $op->matchCount)) {
        $op->matchCount[] = $c;
    }
}

function remove(&$op, $c) {
    $index = array_search($c, $op->matchCount);
    if ($index !== false) {
        array_splice($op->matchCount, $index, 1);
    }
}

function testCode($registers, $result, $instruction, &$opcodes) {
    $sum = 0;
    foreach ($opcodes as $opcode) {
        if (compareArrays($result, runOp($opcode, $registers, $instruction))) {
            add($opcode, $instruction[0]);
            $sum++;
        }
    }
    return $sum;
}

$input = file_get_contents("input.txt");
$inputStr = trim($input);
$lines = explode("\n", $inputStr);

$opcodes = [
    new OP("addr", '+', 'r', 'r'),
    new OP("addi", '+', 'r', 'v'),
    new OP("mulr", '*', 'r', 'r'),
    new OP("muli", '*', 'r', 'v'),
    new OP("banr", '&', 'r', 'r'),
    new OP("bani", '&', 'r', 'v'),
    new OP("borr", '|', 'r', 'r'),
    new OP("bori", '|', 'r', 'v'),
    new OP("setr", 'a', 'r', 'r'),
    new OP("seti", 'a', 'v', 'r'),
    new OP("gtir", '>', 'v', 'r'),
    new OP("gtri", '>', 'r', 'v'),
    new OP("gtrr", '>', 'r', 'r'),
    new OP("eqir", '=', 'v', 'r'),
    new OP("eqri", '=', 'r', 'v'),
    new OP("eqrr", '=', 'r', 'r'),
];

$sum = 0;
$lineCount = 0;
while ($lineCount < count($lines)) {
    if (strlen($lines[$lineCount]) > 0 && $lines[$lineCount][0] === 'B') {
        $split = regSplit($lines[$lineCount], "[^0-9]+");
        $registers = [
            strToInt($split[1]),
            strToInt($split[2]),
            strToInt($split[3]),
            strToInt($split[4]),
        ];
        $split = regSplit($lines[$lineCount + 1], "[^0-9]+");
        $instruction = [
            strToInt($split[0]),
            strToInt($split[1]),
            strToInt($split[2]),
            strToInt($split[3]),
        ];
        $split = regSplit($lines[$lineCount + 2], "[^0-9]+");
        $result = [
            strToInt($split[1]),
            strToInt($split[2]),
            strToInt($split[3]),
            strToInt($split[4]),
        ];
        $tempSum = testCode($registers, $result, $instruction, $opcodes);

        if ($tempSum >= 3) {
            $sum++;
        }

        $lineCount += 4;
    } else {
        break;
    }
}

$orderedOpCodes = [];

while (count($orderedOpCodes) < 16) {
    foreach ($opcodes as $opcode) {
        if (count($opcode->matchCount) === 1) {
            $c = $opcode->matchCount[0];
            $orderedOpCodes[$c] = $opcode;
            foreach ($opcodes as $otherOpcode) {
                remove($otherOpcode, $c);
            }
        }
    }
}

$lineCount += 2;

$r = [0, 0, 0, 0];

while ($lineCount < count($lines)) {
    $split = regSplit($lines[$lineCount], "[^0-9]+");
    $instruction = [
        strToInt($split[0]),
        strToInt($split[1]),
        strToInt($split[2]),
        strToInt($split[3]),
    ];

    $r = runOp($orderedOpCodes[$instruction[0]], $r, $instruction);
    $lineCount++;
}

echo $r[0];

?>