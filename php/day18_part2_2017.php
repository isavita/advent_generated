<?php
function getValue($arg, $registers) {
    if (is_numeric($arg)) {
        return (int)$arg;
    }
    return $registers[$arg] ?? 0;
}

$instructions = file("input.txt", FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
$registers0 = ["p" => 0];
$registers1 = ["p" => 1];
$queue0 = [];
$queue1 = [];
$sendCount1 = 0;
$i0 = $i1 = 0;
$deadlock0 = $deadlock1 = false;

while (!($deadlock0 && $deadlock1)) {
    $deadlock0 = $deadlock1 = true;

    // Program 0
    while ($i0 < count($instructions)) {
        $instruction = explode(" ", $instructions[$i0]);
        $cmd = $instruction[0];
        $arg1 = $instruction[1];

        switch ($cmd) {
            case "snd":
                $queue1[] = getValue($arg1, $registers0);
                break;
            case "set":
                $registers0[$arg1] = getValue($instruction[2], $registers0);
                break;
            case "add":
                $registers0[$arg1] += getValue($instruction[2], $registers0);
                break;
            case "mul":
                $registers0[$arg1] *= getValue($instruction[2], $registers0);
                break;
            case "mod":
                $registers0[$arg1] %= getValue($instruction[2], $registers0);
                break;
            case "rcv":
                if (empty($queue0)) {
                    break 2; // Exit the while loop
                }
                $registers0[$arg1] = array_shift($queue0);
                break;
            case "jgz":
                if (getValue($arg1, $registers0) > 0) {
                    $i0 += getValue($instruction[2], $registers0) - 1;
                }
                break;
        }
        $i0++;
        $deadlock0 = false;
    }

    // Program 1
    while ($i1 < count($instructions)) {
        $instruction = explode(" ", $instructions[$i1]);
        $cmd = $instruction[0];
        $arg1 = $instruction[1];

        switch ($cmd) {
            case "snd":
                $queue0[] = getValue($arg1, $registers1);
                $sendCount1++;
                break;
            case "set":
                $registers1[$arg1] = getValue($instruction[2], $registers1);
                break;
            case "add":
                $registers1[$arg1] += getValue($instruction[2], $registers1);
                break;
            case "mul":
                $registers1[$arg1] *= getValue($instruction[2], $registers1);
                break;
            case "mod":
                $registers1[$arg1] %= getValue($instruction[2], $registers1);
                break;
            case "rcv":
                if (empty($queue1)) {
                    break 2; // Exit the while loop
                }
                $registers1[$arg1] = array_shift($queue1);
                break;
            case "jgz":
                if (getValue($arg1, $registers1) > 0) {
                    $i1 += getValue($instruction[2], $registers1) - 1;
                }
                break;
        }
        $i1++;
        $deadlock1 = false;
    }
}

echo $sendCount1;
?>