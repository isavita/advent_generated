
<?php

$track = [];
$carts = [];

$input = file_get_contents("input.txt");
$lines = explode("\n", $input);

foreach ($lines as $i => $line) {
    $track[] = [];
    foreach (str_split($line) as $j => $s) {
        switch ($s) {
            case '>':
                $track[$i][] = '-';
                $cart = ['x' => $j, 'y' => $i, 'dir' => '>'];
                $carts[] = $cart;
                break;
            case '<':
                $track[$i][] = '-';
                $cart = ['x' => $j, 'y' => $i, 'dir' => '<'];
                $carts[] = $cart;
                break;
            case '^':
                $track[$i][] = '|';
                $cart = ['x' => $j, 'y' => $i, 'dir' => '^'];
                $carts[] = $cart;
                break;
            case 'v':
                $track[$i][] = '|';
                $cart = ['x' => $j, 'y' => $i, 'dir' => 'v'];
                $carts[] = $cart;
                break;
            default:
                $track[$i][] = $s;
                break;
        }
    }
}

$collision = false;
while (!$collision) {
    foreach ($carts as $i => $cart) {
        switch ($cart['dir']) {
            case '>':
                $carts[$i] = movingRight($track, $cart);
                break;
            case '<':
                $carts[$i] = movingLeft($track, $cart);
                break;
            case '^':
                $carts[$i] = movingUp($track, $cart);
                break;
            case 'v':
                $carts[$i] = movingDown($track, $cart);
                break;
            default:
                echo "error not valid cart\n";
                break;
        }
    }

    foreach ($carts as $i => $cart) {
        foreach (array_slice($carts, $i + 1) as $otherCart) {
            if ($cart['x'] == $otherCart['x'] && $cart['y'] == $otherCart['y']) {
                $collision = true;
                echo $cart['x'] . "," . $cart['y'];
            }
        }
    }
}

function movingDown($track, $cart) {
    switch ($track[$cart['y'] + 1][$cart['x']]) {
        case '/':
            $cart['dir'] = '<';
            break;
        case '\\':
            $cart['dir'] = '>';
            break;
        case '+':
            if ($cart['turn'] == 0) {
                $cart['dir'] = '>';
                $cart['turn'] = 1;
            } elseif ($cart['turn'] == 1) {
                $cart['turn'] = 2;
            } elseif ($cart['turn'] == 2) {
                $cart['dir'] = '<';
                $cart['turn'] = 0;
            }
            break;
        case '|':
            break;
        default:
            echo "Error on track cart can't move : " . $cart['x'] . "," . ($cart['y'] - 1) . "," . $track[$cart['y'] - 1][$cart['x']] . "\n";
    }
    $cart['y'] = $cart['y'] + 1;
    return $cart;
}

function movingUp($track, $cart) {
    switch ($track[$cart['y'] - 1][$cart['x']]) {
        case '/':
            $cart['dir'] = '>';
            break;
        case '\\':
            $cart['dir'] = '<';
            break;
        case '+':
            if ($cart['turn'] == 0) {
                $cart['dir'] = '<';
                $cart['turn'] = 1;
            } elseif ($cart['turn'] == 1) {
                $cart['turn'] = 2;
            } elseif ($cart['turn'] == 2) {
                $cart['dir'] = '>';
                $cart['turn'] = 0;
            }
            break;
        case '|':
            break;
        default:
            echo "Error on track cart can't move : " . $cart['x'] . "," . ($cart['y'] - 1) . "," . $track[$cart['y'] - 1][$cart['x']] . "\n";
    }
    $cart['y'] = $cart['y'] - 1;
    return $cart;
}

function movingLeft($track, $cart) {
    switch ($track[$cart['y']][$cart['x'] - 1]) {
        case '/':
            $cart['dir'] = 'v';
            break;
        case '\\':
            $cart['dir'] = '^';
            break;
        case '+':
            if ($cart['turn'] == 0) {
                $cart['dir'] = 'v';
                $cart['turn'] = 1;
            } elseif ($cart['turn'] == 1) {
                $cart['turn'] = 2;
            } elseif ($cart['turn'] == 2) {
                $cart['dir'] = '^';
                $cart['turn'] = 0;
            }
            break;
        case '-':
            break;
        default:
            echo "Error on track cart can't move : " . ($cart['x'] - 1) . "," . $cart['y'] . "," . $track[$cart['y']][$cart['x'] - 1] . "\n";
    }
    $cart['x'] = $cart['x'] - 1;
    return $cart;
}

function movingRight($track, $cart) {
    switch ($track[$cart['y']][$cart['x'] + 1]) {
        case '\\':
            $cart['dir'] = 'v';
            break;
        case '/':
            $cart['dir'] = '^';
            break;
        case '+':
            if ($cart['turn'] == 0) {
                $cart['dir'] = '^';
                $cart['turn'] = 1;
            } elseif ($cart['turn'] == 1) {
                $cart['turn'] = 2;
            } elseif ($cart['turn'] == 2) {
                $cart['dir'] = 'v';
                $cart['turn'] = 0;
            }
            break;
        case '-':
            break;
        default:
            echo "Error on track cart can't move : " . ($cart['x'] + 1) . "," . $cart['y'] . "," . $track[$cart['y']][$cart['x'] + 1] . "\n";
    }
    $cart['x'] = $cart['x'] + 1;
    return $cart;
}
?>
