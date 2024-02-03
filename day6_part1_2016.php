
<?php

$input = file_get_contents("input.txt");
$messages = explode("\n", $input);

$correctedMessage = getCorrectedMessage($messages);
echo $correctedMessage . "\n";

function getCorrectedMessage($messages) {
    if (count($messages) == 0) {
        return "";
    }
    $messageLength = strlen($messages[0]);
    $count = array_fill(0, $messageLength, []);

    foreach ($count as &$c) {
        $c = array();
    }
    unset($c);

    foreach ($messages as $message) {
        for ($j = 0; $j < strlen($message); $j++) {
            $count[$j][$message[$j]] = isset($count[$j][$message[$j]]) ? $count[$j][$message[$j]] + 1 : 1;
        }
    }

    $correctedMessage = "";
    foreach ($count as $charCount) {
        $correctedMessage .= getMostCommonChar($charCount);
    }

    return $correctedMessage;
}

function getMostCommonChar($count) {
    $maxChar = null;
    $maxCount = 0;
    foreach ($count as $char => $cnt) {
        if ($cnt > $maxCount) {
            $maxCount = $cnt;
            $maxChar = $char;
        }
    }
    return $maxChar;
}
?>
