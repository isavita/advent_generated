
<?php

function reverseSection(&$arr, $start, $length) {
    $n = count($arr);
    for ($i = $start, $j = $start + $length - 1; $i < $j; $i++, $j--) {
        $temp = $arr[$i % $n];
        $arr[$i % $n] = $arr[$j % $n];
        $arr[$j % $n] = $temp;
    }
}

function knotHash($input) {
    $lengths = [];
    foreach (str_split($input) as $char) {
        $lengths[] = ord($char);
    }
    $lengths = array_merge($lengths, [17, 31, 73, 47, 23]);

    $list = range(0, 255);

    $position = 0;
    $skip = 0;
    for ($round = 0; $round < 64; $round++) {
        foreach ($lengths as $length) {
            reverseSection($list, $position, $length);
            $position += $length + $skip;
            $skip++;
        }
    }

    $denseHash = [];
    for ($i = 0; $i < 16; $i++) {
        $xor = 0;
        for ($j = 0; $j < 16; $j++) {
            $xor ^= $list[$i * 16 + $j];
        }
        $denseHash[] = $xor;
    }

    $hexHash = '';
    foreach ($denseHash as $v) {
        $hexHash .= sprintf("%02x", $v);
    }

    return $hexHash;
}

function hexToBinary($hexStr) {
    $binaryStr = '';
    foreach (str_split($hexStr) as $hexDigit) {
        $val = hexdec($hexDigit);
        $binaryStr .= sprintf("%04b", $val);
    }
    return $binaryStr;
}

$data = file_get_contents("input.txt");
$keyString = trim($data);
$totalUsed = 0;

for ($i = 0; $i < 128; $i++) {
    $rowKey = $keyString . "-" . $i;
    $hash = knotHash($rowKey);
    $binaryRow = hexToBinary($hash);

    foreach (str_split($binaryRow) as $bit) {
        if ($bit == '1') {
            $totalUsed++;
        }
    }
}

echo $totalUsed . PHP_EOL;
?>
