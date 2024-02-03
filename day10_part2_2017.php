
<?php

$input = file_get_contents("input.txt");

$lengths = [];
for ($i = 0; $i < strlen($input); $i++) {
    $lengths[] = ord($input[$i]);
}
$lengths = array_merge($lengths, [17, 31, 73, 47, 23]);

$list = range(0, 255);
$currentPosition = 0;
$skipSize = 0;

for ($round = 0; $round < 64; $round++) {
    foreach ($lengths as $length) {
        for ($i = 0; $i < $length / 2; $i++) {
            $start = ($currentPosition + $i) % 256;
            $end = ($currentPosition + $length - 1 - $i) % 256;
            [$list[$start], $list[$end]] = [$list[$end], $list[$start]];
        }
        $currentPosition = ($currentPosition + $length + $skipSize) % 256;
        $skipSize++;
    }
}

$denseHash = [];
for ($i = 0; $i < 256; $i += 16) {
    $xor = 0;
    for ($j = 0; $j < 16; $j++) {
        $xor ^= $list[$i + $j];
    }
    $denseHash[] = $xor;
}

$hexHash = bin2hex(call_user_func_array("pack", array_merge(["C*"], $denseHash)));

echo $hexHash . "\n";
?>
