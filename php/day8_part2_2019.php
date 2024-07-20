
<?php
$data = trim(file_get_contents("input.txt"));
$width = 25;
$height = 6;
$layerSize = $width * $height;
$finalImage = array_fill(0, $layerSize, '2');

for ($i = 0; $i < strlen($data); $i += $layerSize) {
    $layer = substr($data, $i, $layerSize);
    for ($j = 0; $j < strlen($layer); $j++) {
        if ($finalImage[$j] === '2') {
            $finalImage[$j] = $layer[$j];
        }
    }
}

echo "Decoded image:\n";
for ($i = 0; $i < $height; $i++) {
    for ($j = 0; $j < $width; $j++) {
        $pixel = $finalImage[$i * $width + $j];
        echo $pixel === '0' ? ' ' : '#';
    }
    echo "\n";
}
?>
