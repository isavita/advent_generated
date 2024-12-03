
<?php

$input = file_get_contents('input.txt');

$re = '/(mul\(\d{1,3},\d{1,3}\))|(do\(\))|(don\'t\(\))/';
preg_match_all($re, $input, $matches, PREG_OFFSET_CAPTURE);

$enabled = true;
$totalSum = 0;

foreach ($matches[0] as $match) {
    $offset = $match[1];
    $instruction = $match[0];

    if (strpos($instruction, 'mul(') === 0) {
        if ($enabled) {
            $nums = explode(',', substr($instruction, 4, -1));
            $totalSum += (int)$nums[0] * (int)$nums[1];
        }
    } elseif (strpos($instruction, 'do(') === 0) {
        $enabled = true;
    } elseif (strpos($instruction, "don't(") === 0) {
        $enabled = false;
    }
}

echo $totalSum;

?>
