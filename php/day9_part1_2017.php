
<?php

$file = fopen("input.txt", "r") or die("Unable to open file!");
$input = fread($file, filesize("input.txt"));
fclose($file);

$score = 0;
$depth = 0;
$inGarbage = false;
$cancelNext = false;

for ($i = 0; $i < strlen($input); $i++) {
    $ch = $input[$i];

    if ($cancelNext) {
        $cancelNext = false;
        continue;
    }

    if ($inGarbage) {
        if ($ch == '!') {
            $cancelNext = true;
        } elseif ($ch == '>') {
            $inGarbage = false;
        }
    } else {
        switch ($ch) {
            case '{':
                $depth++;
                break;
            case '}':
                $score += $depth;
                $depth--;
                break;
            case '<':
                $inGarbage = true;
                break;
        }
    }
}

echo $score;
?>
