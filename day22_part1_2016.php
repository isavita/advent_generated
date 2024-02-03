
<?php

$nodes = readNodes("input.txt");
$viablePairs = countViablePairs($nodes);
echo $viablePairs . PHP_EOL;

function readNodes($filename) {
    $file = fopen($filename, "r");
    if ($file === false) {
        die("Error opening file");
    }

    $nodes = [];
    $nodeRegex = '/node-x\d+-y\d+\s+\d+T\s+(\d+)T\s+(\d+)T\s+\d+%/';
    while (($line = fgets($file)) !== false) {
        if (preg_match($nodeRegex, $line, $matches)) {
            $used = (int)$matches[1];
            $avail = (int)$matches[2];
            $nodes[] = ['used' => $used, 'avail' => $avail];
        }
    }
    fclose($file);
    return $nodes;
}

function countViablePairs($nodes) {
    $count = 0;
    $length = count($nodes);
    for ($i = 0; $i < $length; $i++) {
        for ($j = 0; $j < $length; $j++) {
            if ($i != $j && $nodes[$i]['used'] > 0 && $nodes[$i]['used'] <= $nodes[$j]['avail']) {
                $count++;
            }
        }
    }
    return $count;
}
?>
