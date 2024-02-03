
<?php

$input = file_get_contents("input.txt");
$lengthsStr = explode(",", $input);
$lengths = array_map('intval', $lengthsStr);

$list = range(0, 255);
$currentPosition = 0;
$skipSize = 0;

foreach ($lengths as $length) {
    for ($i = 0; $i < $length / 2; $i++) {
        $start = ($currentPosition + $i) % 256;
        $end = ($currentPosition + $length - 1 - $i) % 256;
        [$list[$start], $list[$end]] = [$list[$end], $list[$start]];
    }

    $currentPosition = ($currentPosition + $length + $skipSize) % 256;
    $skipSize++;
}

$result = $list[0] * $list[1];
echo $result . "\n";
?>
