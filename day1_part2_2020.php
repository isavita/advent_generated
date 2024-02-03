
<?php

$expenses = file("input.txt", FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
foreach ($expenses as $key => $expense) {
    $expenses[$key] = intval($expense);
}

for ($i = 0; $i < count($expenses); $i++) {
    for ($j = $i + 1; $j < count($expenses); $j++) {
        for ($k = $j + 1; $k < count($expenses); $k++) {
            if ($expenses[$i] + $expenses[$j] + $expenses[$k] == 2020) {
                echo $expenses[$i] * $expenses[$j] * $expenses[$k] . "\n";
                exit;
            }
        }
    }
}
?>
