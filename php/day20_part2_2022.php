<?php

class Num {
    public $pos, $val;

    public function __construct($pos, $val) {
        $this->pos = $pos;
        $this->val = $val;
    }
}

function readAll($path) {
    return trim(file_get_contents($path));
}

function toInt($s) {
    return intval($s);
}

function mix(&$nums) {
    $n = count($nums) - 1;
    for ($i = 0; $i < count($nums); $i++) {
        $oldpos = $nums[$i]->pos;
        $newpos = (($oldpos + $nums[$i]->val) % $n + $n) % $n;
        if ($oldpos < $newpos) {
            for ($j = 0; $j < count($nums); $j++) {
                if ($nums[$j]->pos > $oldpos && $nums[$j]->pos <= $newpos) {
                    $nums[$j]->pos--;
                }
            }
        }
        if ($newpos < $oldpos) {
            for ($j = 0; $j < count($nums); $j++) {
                if ($nums[$j]->pos >= $newpos && $nums[$j]->pos < $oldpos) {
                    $nums[$j]->pos++;
                }
            }
        }
        $nums[$i]->pos = $newpos;
    }
}

function coords($nums) {
    $l = count($nums);
    $zeroPos = null;
    for ($i = 0; $i < $l; $i++) {
        if ($nums[$i]->val == 0) {
            $zeroPos = $nums[$i]->pos;
            break;
        }
    }
    $sum = 0;
    for ($i = 0; $i < $l; $i++) {
        if ($nums[$i]->pos == ($zeroPos + 1000) % $l || $nums[$i]->pos == ($zeroPos + 2000) % $l || $nums[$i]->pos == ($zeroPos + 3000) % $l) {
            $sum += $nums[$i]->val;
        }
    }
    return $sum;
}

$nums = [];
foreach (explode("\n", readAll("input.txt")) as $n) {
    $nums[] = new Num(count($nums), toInt($n));
}

$nums2 = array_map(function($n) {
    return new Num($n->pos, 811589153 * $n->val);
}, $nums);

for ($i = 0; $i < 10; $i++) {
    mix($nums2);
}

echo coords($nums2) . "\n";