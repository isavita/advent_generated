
<?php

$file = fopen("input.txt", "r");
$totalCount = 0;
$groupAnswers = [];

while (!feof($file)) {
    $line = trim(fgets($file));
    if ($line == "") {
        $totalCount += count($groupAnswers);
        $groupAnswers = [];
    } else {
        $questions = str_split($line);
        foreach ($questions as $question) {
            $groupAnswers[$question] = true;
        }
    }
}

$totalCount += count($groupAnswers);
echo $totalCount . PHP_EOL;

fclose($file);
?>
