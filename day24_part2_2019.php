
<?php

const Side = 5;
const Square = Side * Side;

function parse() {
    $res = array_fill(0, Square, false);

    $file = fopen("input.txt", "r");
    if ($file === false) {
        die("Error opening file");
    }

    $row = 0;
    while (($line = fgets($file)) !== false) {
        for ($col = 0; $col < Side; $col++) {
            if ($line[$col] === '#') {
                $res[$row * Side + $col] = true;
            }
        }
        $row++;
    }

    fclose($file);

    return $res;
}

function next2($space) {
    $newSpace = [];

    list($minLevel, $maxLevel) = minMaxLevel($space);

    for ($level = $minLevel - 1; $level <= $maxLevel + 1; $level++) {
        $newSpace[$level] = array_fill(0, Square, false);

        for ($cell = 0; $cell < Square; $cell++) {
            if ($cell == 12) {
                continue;
            }

            $row = (int)($cell / Side);
            $col = $cell % Side;
            $neighbours = 0;

            if ($row == 0) {
                if (infested($space, $level - 1, 7)) {
                    $neighbours++;
                }
            }

            if ($col == 0) {
                if (infested($space, $level - 1, 11)) {
                    $neighbours++;
                }
            }

            if ($col == 4) {
                if (infested($space, $level - 1, 13)) {
                    $neighbours++;
                }
            }

            if ($row == 4) {
                if (infested($space, $level - 1, 17)) {
                    $neighbours++;
                }
            }

            if ($cell == 7) {
                for ($i = 0; $i < Side; $i++) {
                    if (infested($space, $level + 1, $i)) {
                        $neighbours++;
                    }
                }
            }

            if ($cell == 11) {
                for ($i = 0; $i < Side; $i++) {
                    if (infested($space, $level + 1, 5 * $i)) {
                        $neighbours++;
                    }
                }
            }

            if ($cell == 13) {
                for ($i = 0; $i < Side; $i++) {
                    if (infested($space, $level + 1, 5 * $i + Side - 1)) {
                        $neighbours++;
                    }
                }
            }

            if ($cell == 17) {
                for ($i = 0; $i < Side; $i++) {
                    if (infested($space, $level + 1, (Side - 1) * Side + $i)) {
                        $neighbours++;
                    }
                }
            }

            if ($row > 0 && $cell != 17) {
                if (infested($space, $level, $cell - Side)) {
                    $neighbours++;
                }
            }

            if ($col > 0 && $cell != 13) {
                if (infested($space, $level, $cell - 1)) {
                    $neighbours++;
                }
            }

            if ($col < Side - 1 && $cell != 11) {
                if (infested($space, $level, $cell + 1)) {
                    $neighbours++;
                }
            }

            if ($row < Side - 1 && $cell != 7) {
                if (infested($space, $level, $cell + Side)) {
                    $neighbours++;
                }
            }

            if (infested($space, $level, $cell) && $neighbours != 1) {
                $newSpace[$level][$cell] = false;
                continue;
            }

            if (!infested($space, $level, $cell) && ($neighbours == 1 || $neighbours == 2)) {
                $newSpace[$level][$cell] = true;
                continue;
            }

            $newSpace[$level][$cell] = infested($space, $level, $cell);
        }
    }

    clean($newSpace);

    return $newSpace;
}

function clean($space) {
    list($min, $max) = minMaxLevel($space);

    $countMin = 0;
    $countMax = 0;
    foreach ($space[$min] as $cell) {
        if ($cell) {
            $countMin++;
        }
    }
    foreach ($space[$max] as $cell) {
        if ($cell) {
            $countMax++;
        }
    }
    if ($countMin == 0) {
        unset($space[$min]);
    }
    if ($countMax == 0) {
        unset($space[$max]);
    }
}

function infested($space, $level, $cell) {
    if (!isset($space[$level])) {
        return false;
    }
    return $space[$level][$cell];
}

function minMaxLevel($space) {
    $min = 999999;
    $max = -999999;
    foreach (array_keys($space) as $level) {
        if ($level < $min) {
            $min = $level;
        }
        if ($level > $max) {
            $max = $level;
        }
    }
    return [$min, $max];
}

$input = parse();

$space = [
    0 => $input
];

for ($i = 0; $i < 200; $i++) {
    $space = next2($space);
}

$count = 0;
foreach ($space as $grid) {
    foreach ($grid as $cell) {
        if ($cell) {
            $count++;
        }
    }
}
echo $count . PHP_EOL;

?>
