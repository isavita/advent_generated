
<?php

function readInput(string $filename): array
{
    $orderingRules = [];
    $updates = [];
    $isUpdateSection = false;

    $file = fopen($filename, 'r');
    if ($file === false) {
        throw new Exception("Error opening file");
    }

    while (($line = fgets($file)) !== false) {
        $line = trim($line);
        if ($line === "") {
            $isUpdateSection = true;
            continue;
        }

        if (!$isUpdateSection) {
            $parts = explode("|", $line);
            if (count($parts) !== 2) {
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

try {
    [$orderingRules, $updates] = readInput("input.txt");
    $sum = 0;
    foreach ($updates as $update) {
        if (isCorrectlyOrdered($update, $orderingRules)) {
            $sum += $update[count($update) / 2];
        }
    }
    echo $sum . PHP_EOL;
} catch (Exception $e) {
    echo "Error: " . $e->getMessage() . PHP_EOL;
}

?>
