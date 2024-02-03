
<?php

$handle = fopen("input.txt", "r");
if ($handle) {
    $count = 0;
    while (($line = fgets($handle)) !== false) {
        $ranges = explode(",", $line);
        if (count($ranges) != 2) {
            continue;
        }
        list($start1, $end1) = explode("-", $ranges[0]);
        list($start2, $end2) = explode("-", $ranges[1]);

        if (($start1 <= $start2 && $end1 >= $end2) || ($start2 <= $start1 && $end2 >= $end1)) {
            $count++;
        }
    }

    fclose($handle);
    echo $count;
} else {
    echo "Error opening file";
}
?>
