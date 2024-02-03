
<?php

$bots = [];
$valueRegex = '/value (\d+) goes to (bot \d+)/';
$givesRegex = '/(bot \d+) gives low to (bot \d+|output \d+) and high to (bot \d+|output \d+)/';

$file = fopen("input.txt", "r") or die("Unable to open file!");

while (($line = fgets($file)) !== false) {

	if (preg_match($valueRegex, $line, $matches)) {
		$value = intval($matches[1]);
		$botID = $matches[2];
		
		if (!array_key_exists($botID, $bots)) {
			$bots[$botID] = ['chips' => []];
		}
		$bots[$botID]['chips'][] = $value;
		
	} elseif (preg_match($givesRegex, $line, $matches)) {
		$botID = $matches[1];
		$lowTo = $matches[2];
		$highTo = $matches[3];
		
		if (!array_key_exists($botID, $bots)) {
			$bots[$botID] = ['chips' => []];
		}
		$bots[$botID]['lowTo'] = $lowTo;
		$bots[$botID]['highTo'] = $highTo;
	}
}

while (true) {
	$action = false;
	foreach ($bots as $botID => $b) {
		if (count($b['chips']) == 2) {
			$action = true;
			[$low, $high] = [min($b['chips']), max($b['chips'])];
			if ($low == 17 && $high == 61) {
				echo $botID . "\n";
				exit;
			}
			$bots[$botID]['chips'] = [];
			
			giveChip($bots, $bots[$botID]['lowTo'], $low);
			giveChip($bots, $bots[$botID]['highTo'], $high);
		}
	}
	if (!$action) {
		break;
	}
}

function giveChip(&$bots, $target, $value) {
	if (!array_key_exists($target, $bots)) {
		$bots[$target] = ['chips' => []];
	}
	$bots[$target]['chips'][] = $value;
}
