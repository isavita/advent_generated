
<?php

function readInput(string $filename): array
{
    $file = fopen($filename, 'r');
    if (!$file) {
        throw new Exception("Error opening file");
    }

    $orderingRules = [];
    $updates = [];
    $isUpdateSection = false;

    while (($line = fgets($file)) !== false) {
        $line = trim($line);
        if ($line === "") {
            $isUpdateSection = true;
            continue;
        }

        if (!$isUpdateSection) {
            $parts = explode("|", $line);
            if (count($parts) != 2) {
                continue;
            }
            $x = (int)trim($parts[0]);
            $y = (int)trim($parts[1]);
            $orderingRules[] = [$x, $y];
        } else {
            $nums = explode(",", $line);
            $update = [];
            foreach ($nums as $numStr) {
                $num = (int)trim($numStr);
                $update[] = $num;
            }
            if (!empty($update)) {
                $updates[] = $update;
            }
        }
    }

    fclose($file);
    return [$orderingRules, $updates];
}

function isCorrectlyOrdered(array $update, array $rules): bool
{
    $position = array_flip($update);
    foreach ($rules as $rule) {
        $x = $rule[0];
        $y = $rule[1];
        if (isset($position[$x]) && isset($position[$y])) {
            if ($position[$x] >= $position[$y]) {
                return false;
            }
        }
    }
    return true;
}


function sortUpdate(array $update, array $rules): array
{
    $adjacency = [];
    $pagesInUpdate = array_flip($update);
    foreach ($update as $page) {
        $adjacency[$page] = [];
    }
    foreach ($rules as $rule) {
        $x = $rule[0];
        $y = $rule[1];
        if (isset($pagesInUpdate[$x]) && isset($pagesInUpdate[$y])) {
            $adjacency[$x][] = $y;
        }
    }

    $visited = [];
    $result = [];
    $recursiveSort = function($n) use (&$visited, &$adjacency, &$result, &$recursiveSort){
        if(isset($visited[$n])){
            return;
        }
        $visited[$n] = true;
        foreach($adjacency[$n] as $m){
            $recursiveSort($m);
        }
        $result[] = $n;
    };

    foreach($pagesInUpdate as $page => $v){
        if(!isset($visited[$page])){
            $recursiveSort($page);
        }
    }
    return array_reverse($result);
}


try {
    [$orderingRules, $updates] = readInput("input.txt");
    $sum = 0;
    foreach ($updates as $update) {
        if (!isCorrectlyOrdered($update, $orderingRules)) {
            $sortedUpdate = sortUpdate($update, $orderingRules);
            $sum += $sortedUpdate[count($sortedUpdate) / 2];
        }
    }
    echo $sum . PHP_EOL;
} catch (Exception $e) {
    echo "Error: " . $e->getMessage() . PHP_EOL;
}

?>
