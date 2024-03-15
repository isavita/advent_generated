<?php
$file = fopen("input.txt", "r");
$vals = [];
while (($line = fgets($file)) !== false) {
    $val = intval(trim($line));
    $vals[] = $val;
}
fclose($file);

$prevSum = $vals[0] + $vals[1] + $vals[2];
$count = 0;
for ($i = 3; $i < count($vals); $i++) {
    $currSum = $vals[$i - 2] + $vals[$i - 1] + $vals[$i];
    if ($currSum > $prevSum) {
        $count++;
    }
    $prevSum = $currSum;
}

echo $count . "\n";
?>