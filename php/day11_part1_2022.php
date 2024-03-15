<?php

class Monkey {
    public $items = [];
    public $operation;
    public $div;
    public $next = [0, 0];
    public $inspections = 0;

    public function parse($s) {
        $lines = explode("\n", $s);
        $this->items = array_map('intval', explode(", ", explode(": ", $lines[1])[1]));
        $formula = explode(" ", explode("= ", $lines[2])[1]);
        $this->operation = function($old) use ($formula) {
            switch ($formula[1]) {
                case '+':
                    return $formula[2] === 'old' ? $old + $old : $old + intval($formula[2]);
                case '*':
                    return $formula[2] === 'old' ? $old * $old : $old * intval($formula[2]);
            }
        };
        sscanf($lines[3], " Test: divisible by %d", $this->div);
        sscanf($lines[4], " If true: throw to monkey %d", $this->next[0]);
        sscanf($lines[5], " If false: throw to monkey %d", $this->next[1]);
        return $this;
    }
}

function monkeyBusiness($monkeys, $rounds, $worry) {
    $div = 1;
    foreach ($monkeys as $m) {
        $div *= $m->div;
    }

    for ($i = 0; $i < $rounds; $i++) {
        foreach ($monkeys as $m) {
            while ($m->items) {
                $m->inspections++;
                $item = call_user_func($m->operation, array_shift($m->items));
                if ($worry) {
                    $item %= $div;
                } else {
                    $item = intdiv($item, 3);
                }
                if ($item % $m->div == 0) {
                    $monkeys[$m->next[0]]->items[] = $item;
                } else {
                    $monkeys[$m->next[1]]->items[] = $item;
                }
            }
        }
    }

    $inspections = [];
    foreach ($monkeys as $m) {
        $inspections[] = $m->inspections;
    }
    rsort($inspections);
    return $inspections[0] * $inspections[1];
}

$monkeys = [];
$s = file_get_contents('input.txt');
foreach (explode("\n\n", $s) as $m) {
    $monkeys[] = (new Monkey)->parse($m);
}
echo monkeyBusiness($monkeys, 20, false);