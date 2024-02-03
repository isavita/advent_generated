
<?php

$inputData = file_get_contents("input.txt");
$inputData = explode(",", $inputData);

$inputData[1] = 12;
$inputData[2] = 2;

$result = executeProgram($inputData);

echo $result . PHP_EOL;

function executeProgram($data) {
    for ($i = 0; $i < count($data) - 3; $i += 4) {
        $pos1 = $data[$i + 1];
        $pos2 = $data[$i + 2];
        $pos3 = $data[$i + 3];
        switch ($data[$i]) {
            case 1:
                $sum = $data[$pos1] + $data[$pos2];
                $data[$pos3] = $sum;
                break;
            case 2:
                $product = $data[$pos1] * $data[$pos2];
                $data[$pos3] = $product;
                break;
            case 99:
                return $data[0];
            default:
                throw new Exception("Invalid opcode");
        }
    }

    return $data[0];
}
?>
