
<?php

$data = file_get_contents("input.txt");
$frequencyChanges = explode("\n", trim($data));
$frequencies = array();
$currentFrequency = 0;
$frequencies[$currentFrequency] = true;

while (true) {
    foreach ($frequencyChanges as $change) {
        $frequencyDelta = intval($change);
        $currentFrequency += $frequencyDelta;
        if (isset($frequencies[$currentFrequency])) {
            echo $currentFrequency . "\n";
            return;
        }
        $frequencies[$currentFrequency] = true;
    }
}
?>
