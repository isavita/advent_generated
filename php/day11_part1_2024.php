
<?php
$line = trim(file_get_contents("input.txt"));
$stones = explode(" ", $line);

for ($i = 0; $i < 25; $i++) {
    $next = [];
    foreach ($stones as $s) {
        if ($s === "0") {
            $next[] = "1";
        } else if (strlen($s) % 2 === 0) {
            $mid = strlen($s) / 2;
            $left = ltrim(substr($s, 0, $mid), '0');
            $right = ltrim(substr($s, $mid), '0');
            if ($left === "") $left = "0";
            if ($right === "") $right = "0";
            $next[] = $left;
            $next[] = $right;
        } else {
            $next[] = (string)((int)$s * 2024);
        }
    }
    $stones = $next;
}

echo count($stones) . PHP_EOL;
?>
