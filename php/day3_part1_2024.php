
<?php

$input = file_get_contents('input.txt');

preg_match_all('/mul\((\d{1,3}),(\d{1,3})\)/', $input, $matches);

$totalSum = 0;
foreach ($matches[0] as $key => $match) {
    $totalSum += (int)$matches[1][$key] * (int)$matches[2][$key];
}

echo $totalSum;

?>
