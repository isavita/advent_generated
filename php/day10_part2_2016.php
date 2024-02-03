
<?php

$lines = file("input.txt", FILE_IGNORE_NEW_LINES);
$bots = [];
$outputs = [];
$valueRegex = '/value (\d+) goes to (bot \d+)/';
$givesRegex = '/(bot \d+) gives low to (bot \d+|output \d+) and high to (bot \d+|output \d+)/';

foreach ($lines as $line) {
    if (preg_match($valueRegex, $line, $matches)) {
        $value = intval($matches[1]);
        $botID = $matches[2];

        if (!array_key_exists($botID, $bots)) {
            $bots[$botID] = ['lowTo' => '', 'highTo' => '', 'chips' => []];
        }
        $bots[$botID]['chips'][] = $value;

    } elseif (preg_match($givesRegex, $line, $matches)) {
        $botID = $matches[1];
        $lowTo = $matches[2];
        $highTo = $matches[3];

        if (!array_key_exists($botID, $bots)) {
            $bots[$botID] = ['lowTo' => '', 'highTo' => '', 'chips' => []];
        }
        $bots[$botID]['lowTo'] = $lowTo;
        $bots[$botID]['highTo'] = $highTo;
    }
}

while (true) {
    $action = false;
    foreach ($bots as $botID => $bot) {
        if (count($bot['chips']) == 2) {
            $action = true;
            list($low, $high) = [min($bot['chips'][0], $bot['chips'][1]), max($bot['chips'][0], $bot['chips'][1])];
            $bots[$botID]['chips'] = [];

            giveChip($bots, $outputs, $bot['lowTo'], $low);
            giveChip($bots, $outputs, $bot['highTo'], $high);
        }
    }
    if (!$action) {
        break;
    }
}

$result = $outputs['output 0'] * $outputs['output 1'] * $outputs['output 2'];
echo $result . PHP_EOL;

function giveChip(&$bots, &$outputs, $target, $value) {
    if (strpos($target, 'bot') === 0) {
        if (!array_key_exists($target, $bots)) {
            $bots[$target] = ['lowTo' => '', 'highTo' => '', 'chips' => []];
        }
        $bots[$target]['chips'][] = $value;
    } elseif (strpos($target, 'output') === 0) {
        $outputs[$target] = $value;
    }
}
