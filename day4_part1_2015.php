
<?php

$data = file_get_contents("input.txt");
$secretKey = trim($data);
$number = 0;

while (true) {
    $hash = md5($secretKey . $number);
    $hashString = $hash;

    if (substr($hashString, 0, 5) === "00000") {
        echo $number . "\n";
        break;
    }
    $number++;
}
?>
