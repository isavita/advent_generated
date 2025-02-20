
<?php

function solve() {
    $register5 = 0;
    $seen = [];
    $last_unique = 0;

    while (true) {
        $register3 = $register5 | 65536;
        $register5 = 7586220;

        while (true) {
            $register1 = $register3 & 255;
            $register5 = ((($register5 + $register1) & 16777215) * 65899) & 16777215;

            if ($register3 < 256) {
                if (isset($seen[$register5])) {
                    print($last_unique);
                    return;
                }
                $seen[$register5] = true;
                $last_unique = $register5;
                break;
            } else {
                $register3 = (int)($register3 / 256);
            }
        }
    }
}

solve();
?>
