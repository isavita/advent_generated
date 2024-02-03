
<?php

$input = file_get_contents("input.txt");
$decompressedLength = getDecompressedLength($input);

echo $decompressedLength . PHP_EOL;

function getDecompressedLength($input) {
    $markerRegex = '/\((\d+)x(\d+)\)/';
    $length = 0;
    $i = 0;
    while ($i < strlen($input)) {
        if (preg_match($markerRegex, $input, $matches, 0, $i)) {
            $charCount = intval($matches[1]);
            $repeatCount = intval($matches[2]);
            $length += $charCount * $repeatCount;
            $i += strlen($matches[0]) + $charCount;
        } else {
            $length++;
            $i++;
        }
    }
    return $length;
}
?>
