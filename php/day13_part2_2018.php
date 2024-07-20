
<?php

class Cart {
    public $x, $y, $direction, $turns;

    public function __construct($x, $y, $direction) {
        $this->x = $x;
        $this->y = $y;
        $this->direction = $direction;
        $this->turns = 0;
    }
}

$tracks = [];
$carts = [];
$input = file('input.txt', FILE_IGNORE_NEW_LINES);

foreach ($input as $y => $line) {
    $trackLine = [];
    for ($x = 0; $x < strlen($line); $x++) {
        $r = $line[$x];
        if (in_array($r, ['>', '<', '^', 'v'])) {
            $carts[] = new Cart($x, $y, $r);
            $trackLine[$x] = ($r == '>' || $r == '<') ? '-' : '|';
        } else {
            $trackLine[$x] = $r;
        }
    }
    $tracks[] = $trackLine;
}

while (count($carts) > 1) {
    usort($carts, function($a, $b) {
        return $a->y === $b->y ? $a->x - $b->x : $a->y - $b->y;
    });

    $toRemove = [];
    foreach ($carts as $i => $cart) {
        if (in_array($i, $toRemove)) continue;

        moveCart($cart, $tracks);
        if (($crashIndex = checkCrash($cart, $carts)) !== -1) {
            $toRemove[] = $i;
            $toRemove[] = $crashIndex;
        }
    }

    $carts = array_values(array_diff_key($carts, array_flip($toRemove)));
}

echo "{$carts[0]->x},{$carts[0]->y}";

function moveCart($cart, &$tracks) {
    switch ($cart->direction) {
        case '>': $cart->x++; break;
        case '<': $cart->x--; break;
        case '^': $cart->y--; break;
        case 'v': $cart->y++; break;
    }

    switch ($tracks[$cart->y][$cart->x]) {
        case '+': turnCart($cart); break;
        case '/': changeDirection($cart, '/'); break;
        case '\\': changeDirection($cart, '\\'); break;
    }
}

function turnCart($cart) {
    $turns = $cart->turns % 3;
    if ($turns === 0) {
        $cart->direction = ($cart->direction === '>') ? '^' : (($cart->direction === '<') ? 'v' : (($cart->direction === '^') ? '<' : '>'));
    } elseif ($turns === 2) {
        $cart->direction = ($cart->direction === '>') ? 'v' : (($cart->direction === '<') ? '^' : (($cart->direction === '^') ? '>' : '<'));
    }
    $cart->turns++;
}

function changeDirection($cart, $track) {
    if ($track === '/') {
        $cart->direction = ($cart->direction === '>') ? '^' : (($cart->direction === '<') ? 'v' : (($cart->direction === '^') ? '>' : '<'));
    } else {
        $cart->direction = ($cart->direction === '>') ? 'v' : (($cart->direction === '<') ? '^' : (($cart->direction === '^') ? '<' : '>'));
    }
}

function checkCrash($cart, $carts) {
    foreach ($carts as $i => $c) {
        if ($c !== $cart && $c->x === $cart->x && $c->y === $cart->y) {
            return $i;
        }
    }
    return -1;
}
