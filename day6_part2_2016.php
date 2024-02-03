
<?php

$file = fopen("input.txt", "r");
$messages = [];
while (!feof($file)) {
    $messages[] = trim(fgets($file));
}
fclose($file);

function getOriginalMessage($messages) {
    if (empty($messages)) {
        return "";
    }
    $messageLength = strlen($messages[0]);
    $count = [];
    for ($i = 0; $i < $messageLength; $i++) {
        $count[$i] = [];
    }

    foreach ($messages as $message) {
        for ($j = 0; $j < $messageLength; $j++) {
            $char = $message[$j];
            if (!isset($count[$j][$char])) {
                $count[$j][$char] = 1;
            } else {
                $count[$j][$char]++;
            }
        }
    }

    $originalMessage = "";
    foreach ($count as $charCount) {
        $originalMessage .= getLeastCommonChar($charCount);
    }

    return $originalMessage;
}

function getLeastCommonChar($count) {
    $minChar = null;
    $minCount = PHP_INT_MAX;
    foreach ($count as $char => $cnt) {
        if ($cnt < $minCount) {
            $minCount = $cnt;
            $minChar = $char;
        }
    }
    return $minChar;
}

$originalMessage = getOriginalMessage($messages);
echo $originalMessage . PHP_EOL;
?>
