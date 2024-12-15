
<?php
$line = trim(file_get_contents("input.txt"));
$disk = [];
$fileID = 0;
$isFile = true;
$len = strlen($line);

for ($i = 0; $i < $len; $i++) {
    $length = (int)$line[$i];
    if ($isFile) {
        $disk = array_merge($disk, array_fill(0, $length, $fileID));
        $fileID++;
    } else {
        $disk = array_merge($disk, array_fill(0, $length, -1));
    }
    $isFile = !$isFile;
}

$files = [];
$curID = -1;
$start = 0;
$diskLen = count($disk);

for ($i = 0; $i < $diskLen; $i++) {
    if ($disk[$i] === -1) {
        $curID = -1;
        continue;
    }
    $id = $disk[$i];
    if ($id !== $curID) {
        $curID = $id;
        $start = $i;
    }
    if ($i === $diskLen - 1 || ($i + 1 < $diskLen && $disk[$i + 1] !== $id)) {
        $files[] = ['id' => $id, 'start' => $start, 'end' => $i];
    }
}

for ($i = count($files) - 1; $i >= 0; $i--) {
    $f = $files[$i];
    $fileLen = $f['end'] - $f['start'] + 1;
    $leftmostSpan = -1;
    $spanLen = 0;
    for ($j = 0; $j < $f['start']; $j++) {
        if ($disk[$j] === -1) {
            if ($spanLen === 0) {
                $leftmostSpan = $j;
            }
            $spanLen++;
            if ($spanLen === $fileLen) {
                break;
            }
        } else {
            $spanLen = 0;
            $leftmostSpan = -1;
        }
    }
    if ($leftmostSpan !== -1 && $spanLen === $fileLen) {
        for ($x = $f['start']; $x <= $f['end']; $x++) {
            $disk[$x] = -1;
        }
        for ($x = 0; $x < $fileLen; $x++) {
            $disk[$leftmostSpan + $x] = $f['id'];
        }
    }
}

$checksum = 0;
for ($i = 0; $i < $diskLen; $i++) {
    if ($disk[$i] !== -1) {
        $checksum += $i * $disk[$i];
    }
}

echo $checksum . PHP_EOL;
?>
