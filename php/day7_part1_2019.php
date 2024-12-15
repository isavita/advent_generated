
<?php

function getParam(array $code, int $address, bool $immediate): int {
    $param = $code[$address];
    return $immediate ? $param : $code[$param];
}

function runVM(array $code, array $input): int {
    $ip = 0;
    $output = 0;
    $inputIndex = 0;

    while (true) {
        $cmd = $code[$ip];
        $opCode = $cmd % 100;

        switch ($opCode) {
            case 1:
                $param1 = getParam($code, $ip + 1, (int)($cmd / 100) % 10 == 1);
                $param2 = getParam($code, $ip + 2, (int)($cmd / 1000) % 10 == 1);
                $address = $code[$ip + 3];
                $code[$address] = $param1 + $param2;
                $ip += 4;
                break;
            case 2:
                $param1 = getParam($code, $ip + 1, (int)($cmd / 100) % 10 == 1);
                $param2 = getParam($code, $ip + 2, (int)($cmd / 1000) % 10 == 1);
                $address = $code[$ip + 3];
                $code[$address] = $param1 * $param2;
                $ip += 4;
                break;
            case 3:
                $address = $code[$ip + 1];
                $code[$address] = $input[$inputIndex++];
                $ip += 2;
                break;
            case 4:
                $output = getParam($code, $ip + 1, (int)($cmd / 100) % 10 == 1);
                $ip += 2;
                break;
            case 5:
                $param1 = getParam($code, $ip + 1, (int)($cmd / 100) % 10 == 1);
                $param2 = getParam($code, $ip + 2, (int)($cmd / 1000) % 10 == 1);
                $ip = $param1 != 0 ? $param2 : $ip + 3;
                break;
            case 6:
                $param1 = getParam($code, $ip + 1, (int)($cmd / 100) % 10 == 1);
                $param2 = getParam($code, $ip + 2, (int)($cmd / 1000) % 10 == 1);
                $ip = $param1 == 0 ? $param2 : $ip + 3;
                break;
            case 7:
                $param1 = getParam($code, $ip + 1, (int)($cmd / 100) % 10 == 1);
                $param2 = getParam($code, $ip + 2, (int)($cmd / 1000) % 10 == 1);
                $address = $code[$ip + 3];
                $code[$address] = $param1 < $param2 ? 1 : 0;
                $ip += 4;
                break;
            case 8:
                $param1 = getParam($code, $ip + 1, (int)($cmd / 100) % 10 == 1);
                $param2 = getParam($code, $ip + 2, (int)($cmd / 1000) % 10 == 1);
                $address = $code[$ip + 3];
                $code[$address] = $param1 == $param2 ? 1 : 0;
                $ip += 4;
                break;
            case 99:
                return $output;
            default:
                throw new Exception("Invalid opcode: " . $opCode);
        }
    }
}

function permutations(array $arr): array {
    $result = [];
    $n = count($arr);

    function generate(array $arr, int $n, array &$result) {
        if ($n === 1) {
            $result[] = $arr;
            return;
        }

        for ($i = 0; $i < $n; $i++) {
            generate($arr, $n - 1, $result);
            if ($n % 2 === 1) {
                $temp = $arr[$i];
                $arr[$i] = $arr[$n - 1];
                $arr[$n - 1] = $temp;
            } else {
                $temp = $arr[0];
                $arr[0] = $arr[$n - 1];
                $arr[$n - 1] = $temp;
            }
        }
    }

    generate($arr, $n, $result);
    return $result;
}

$code = array_map('intval', explode(',', trim(file_get_contents('input.txt'))));
$max = 0;

foreach (permutations([0, 1, 2, 3, 4]) as $phase) {
    $input = [0];
    $output = 0;
    for ($i = 0; $i < 5; $i++) {
        $input = array_merge([$phase[$i]], $input);
        $output = runVM($code, $input);
        $input = [$output];
    }
    $max = max($max, $output);
}

echo $max . PHP_EOL;
