<?php

class Num {
    public $pos;
    public $val;

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
    foreach ($nums as $i => $num) {
        $oldpos = $num->pos;
        $newpos = (($oldpos + $num->val) % $n + $n) % $n;
        if ($oldpos < $newpos) {
            foreach ($nums as $j => $other) {
                if ($other->pos > $oldpos && $other->pos <= $newpos) {
                    $nums[$j]->pos--;
                }
            }
        }
        if ($newpos < $oldpos) {
            foreach ($nums as $j => $other) {
                if ($other->pos >= $newpos && $other->pos < $oldpos) {
                    $nums[$j]->pos++;
                }
            }
        }
        $nums[$i]->pos = $newpos;
    }
}

function coords($nums) {
    $l = count($nums);
    $zeroPos = 0;
    foreach ($nums as $num) {
        if ($num->val == 0) {
            $zeroPos = $num->pos;
            break;
        }
    }
    $sum = 0;
    foreach ($nums as $num) {
        if ($num->pos == ($zeroPos + 1000) % $l || $num->pos == ($zeroPos + 2000) % $l || $num->pos == ($zeroPos + 3000) % $l) {
            $sum += $num->val;
        }
    }
    return $sum;
}

$nums = [];
foreach (explode("\n", readAll("input.txt")) as $i => $n) {
    $nums[] = new Num($i, toInt($n));
}
$nums2 = [];
foreach ($nums as $i => $num) {
    $nums2[] = new Num($num->pos, 811589153 * $num->val);
}

mix($nums);
echo coords($nums);

?>