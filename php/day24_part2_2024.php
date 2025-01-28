
<?php
function parse(string $input): ?array {
    $parts = explode("\n\n", $input);
    if (count($parts) !== 2) {
        return null;
    }

    $gates = [];
    $lines = explode("\n", $parts[1]);
    foreach ($lines as $line) {
        if ($line === "") {
            continue;
        }
        $parts_line = explode(" -> ", $line);
        if (count($parts_line) !== 2) {
            continue;
        }
        $gateParts = explode(" ", $parts_line[0]);
        if (count($gateParts) !== 3) {
            continue;
        }
        $gates[] = [
            'gate' => ['a' => $gateParts[0], 'op' => $gateParts[1], 'b' => $gateParts[2]],
            'output' => $parts_line[1],
        ];
    }
    return $gates;
}

function createLookups(array $gates): array {
    $lookup = [];
    $reverseLookup = [];

    foreach ($gates as $g) {
        $lookup[$g['output']] = $g['gate'];
        $inputs = [$g['gate']['a'], $g['gate']['b']];
        sort($inputs);
        $key = sprintf("%s_%s_%s", $inputs[0], $g['gate']['op'], $inputs[1]);
        $reverseLookup[$key] = $g['output'];
    }
    return [$lookup, $reverseLookup];
}

function swap(array &$pairs, array &$gates, string $a, string $b): void {
    $pairs[] = [$a, $b];
    for ($i = 0; $i < count($gates); $i++) {
        if ($gates[$i]['output'] === $a) {
            $gates[$i]['output'] = $b;
        } else if ($gates[$i]['output'] === $b) {
            $gates[$i]['output'] = $a;
        }
    }
}

function getReverseLookupKey(string $a, string $op, string $b): string {
    $inputs = [$a, $b];
    sort($inputs);
    return sprintf("%s_%s_%s", $inputs[0], $op, $inputs[1]);
}

function solution(array $gates): string {
    $pairs = [];
    $numZ = 0;
    foreach ($gates as $g) {
        if (strpos($g['output'], "z") === 0) {
            $numZ++;
        }
    }

    while (count($pairs) < 4) {
        $adder = "";
        $carry = "";
        [$lookup, $reverseLookup] = createLookups($gates);

        for ($i = 0; $i < $numZ; $i++) {
            $xi = sprintf("x%02d", $i);
            $yi = sprintf("y%02d", $i);
            $zi = sprintf("z%02d", $i);

            if ($i === 0) {
                $adder = $reverseLookup[getReverseLookupKey($xi, "XOR", $yi)] ?? "";
                $carry = $reverseLookup[getReverseLookupKey($xi, "AND", $yi)] ?? "";
            } else {
                $bit = $reverseLookup[getReverseLookupKey($xi, "XOR", $yi)] ?? "";
                if ($bit !== "") {
                    $adder = $reverseLookup[getReverseLookupKey($bit, "XOR", $carry)] ?? "";
                    if ($adder !== "") {
                        $c1 = $reverseLookup[getReverseLookupKey($xi, "AND", $yi)] ?? "";
                        $c2 = $reverseLookup[getReverseLookupKey($bit, "AND", $carry)] ?? "";
                        $carry = $reverseLookup[getReverseLookupKey($c1, "OR", $c2)] ?? "";
                    }
                }
            }

            if ($adder === "") {
                $gate = $lookup[$zi];
                $bitKey = getReverseLookupKey($xi, "XOR", $yi);
                $bit = $reverseLookup[$bitKey] ?? "";
                if (isset($reverseLookup[getReverseLookupKey($gate['a'], "XOR", $carry)])) {
                    swap($pairs, $gates, $bit, $gate['a']);
                    break;
                } else if (isset($reverseLookup[getReverseLookupKey($gate['b'], "XOR", $carry)])) {
                    swap($pairs, $gates, $bit, $gate['b']);
                    break;
                }
            } else if ($adder !== $zi) {
                swap($pairs, $gates, $adder, $zi);
                break;
            }
        }
    }

    $result = [];
    foreach ($pairs as $pair) {
        $result[] = $pair[0];
        $result[] = $pair[1];
    }
    sort($result);
    return implode(",", $result);
}

$input = file_get_contents("input.txt");
$gates = parse($input);
if ($gates === null) {
    echo "Error parsing input\n";
} else {
    echo solution($gates) . "\n";
}
?>
