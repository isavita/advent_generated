<?php
$file = fopen("input.txt", "r");
$input = fread($file, filesize("input.txt"));
fclose($file);

echo getDecompressedLength($input);

function getDecompressedLength($input) {
    $length = 0;
    $i = 0;
    $end = strlen($input);
    while ($i < $end) {
        if (preg_match('/\((\d+)x(\d+)\)/', $input, $matches, PREG_OFFSET_CAPTURE, $i)) {
            $charCount = (int)$matches[1][0];
            $repeatCount = (int)$matches[2][0];
            $nextIndex = $matches[0][1] + strlen($matches[0][0]);
            $length += $repeatCount * getDecompressedLength(substr($input, $nextIndex, $charCount));
            $i = $nextIndex + $charCount;
        } else {
            $length++;
            $i++;
        }
    }
    return $length;
}
?>