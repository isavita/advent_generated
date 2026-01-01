
<?php
$tokens = preg_split('/\s+/', trim(file_get_contents('input.txt')));
$pos = 50;
$cnt = 0;
foreach ($tokens as $t) {
    $dir = $t[0];
    $amt = (int)substr($t, 1);
    $pos = ($pos + ($dir === 'R' ? $amt : -$amt)) % 100;
    if ($pos < 0) $pos += 100;
    if ($pos === 0) $cnt++;
}
echo $cnt . PHP_EOL;
?>
