
<?php

$file = fopen("input.txt", "r");
$jobs = [];
$results = [];

while (!feof($file)) {
    $line = trim(fgets($file));
    $parts = explode(": ", $line);
    $jobs[$parts[0]] = $parts[1];
}

fclose($file);

echo calculate("root", $jobs, $results) . PHP_EOL;

function calculate($monkey, $jobs, &$results) {
    if (array_key_exists($monkey, $results)) {
        return $results[$monkey];
    }

    if (!array_key_exists($monkey, $jobs)) {
        throw new Exception("Monkey not found: " . $monkey);
    }

    $job = $jobs[$monkey];

    if (is_numeric($job)) {
        $results[$monkey] = $job;
        return $job;
    }

    $parts = explode(" ", $job);
    $a = calculate($parts[0], $jobs, $results);
    $b = calculate($parts[2], $jobs, $results);

    switch ($parts[1]) {
        case "+":
            $result = $a + $b;
            break;
        case "-":
            $result = $a - $b;
            break;
        case "*":
            $result = $a * $b;
            break;
        case "/":
            $result = $a / $b;
            break;
        default:
            throw new Exception("Unknown operation: " . $parts[1]);
    }

    $results[$monkey] = $result;
    return $result;
}
?>
