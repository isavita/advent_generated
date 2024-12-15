
<?php

function parseInput(array $input): array {
    $workflows = [];
    $parts = [];

    $i = 0;
    for (; $input[$i] !== ""; $i++) {
        [$workflowName, $rules] = parseWorkflow($input[$i]);
        $workflows[$workflowName] = $rules;
    }

    for ($i = $i + 1; $i < count($input); $i++) {
        $parts[] = parsePart($input[$i]);
    }

    return [$workflows, $parts];
}

function parseWorkflow(string $line): array {
    $idx = strpos($line, "{");
    $workflowName = substr($line, 0, $idx);
    $rules = [];
    $rulesStr = explode(",", substr($line, $idx + 1, -1));

    foreach ($rulesStr as $ruleStr) {
        $rule = [];
        $idx = strpos($ruleStr, ":");
        if ($idx === false) {
            $rule['WorkflowName'] = $ruleStr;
        } else {
            $rule['Category'] = $ruleStr[0];
            $rule['Operator'] = $ruleStr[1];
            $rule['Num'] = (int)substr($ruleStr, 2, $idx - 2);
            $rule['WorkflowName'] = substr($ruleStr, $idx + 1);
        }
        $rules[] = $rule;
    }
    return [$workflowName, $rules];
}

function parsePart(string $line): array {
    preg_match('/\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)\}/', $line, $matches);
    return [
        'x' => (int)$matches[1],
        'm' => (int)$matches[2],
        'a' => (int)$matches[3],
        's' => (int)$matches[4],
    ];
}

function applyWorkflow(array $part, array $workflows, string $workflowName): bool {
    if ($workflowName === "A") {
        return true;
    }
    if ($workflowName === "R") {
        return false;
    }

    foreach ($workflows[$workflowName] as $rule) {
        $rating = $part[$rule['Category']];
        $isValid = true;
        switch ($rule['Operator']) {
            case '>':
                $isValid = $rating > $rule['Num'];
                break;
            case '<':
                $isValid = $rating < $rule['Num'];
                break;
        }

        if ($isValid) {
            return applyWorkflow($part, $workflows, $rule['WorkflowName']);
        }
    }
    return false;
}

function applyWorkflowInterval(array $partInterval, array $workflows, string $workflowName): int {
    if ($workflowName === "A") {
        $res = 1;
        foreach ($partInterval as $interval) {
            $res *= ($interval['End'] - $interval['Start'] + 1);
        }
        return $res;
    }
    if ($workflowName === "R") {
        return 0;
    }

    $res = 0;
    foreach ($workflows[$workflowName] as $rule) {
        $ratingInterval = $partInterval[$rule['Category']];
        $validRatingInterval = [];
        $invalidRatingInterval = [];

        switch ($rule['Operator']) {
            case '>':
                $invalidRatingInterval = ['Start' => $ratingInterval['Start'], 'End' => $rule['Num']];
                $validRatingInterval = ['Start' => $rule['Num'] + 1, 'End' => $ratingInterval['End']];
                break;
            case '<':
                $validRatingInterval = ['Start' => $ratingInterval['Start'], 'End' => $rule['Num'] - 1];
                $invalidRatingInterval = ['Start' => $rule['Num'], 'End' => $ratingInterval['End']];
                break;
            default:
                $validRatingInterval = $ratingInterval;
        }

        $newPart = [];
        foreach ($partInterval as $key => $value) {
            if ($key === $rule['Category']) {
                $newPart[$key] = $validRatingInterval;
            } else {
                $newPart[$key] = $value;
            }
        }
        $res += applyWorkflowInterval($newPart, $workflows, $rule['WorkflowName']);
        $partInterval[$rule['Category']] = $invalidRatingInterval;
    }
    return $res;
}

function solve(array $input): int {
    $startWorflow = "in";
    $minRating = 1;
    $maxRating = 4000;

    [$workflows, $_] = parseInput($input);
    $partInterval = [
        'x' => ['Start' => $minRating, 'End' => $maxRating],
        'm' => ['Start' => $minRating, 'End' => $maxRating],
        'a' => ['Start' => $minRating, 'End' => $maxRating],
        's' => ['Start' => $minRating, 'End' => $maxRating],
    ];

    return applyWorkflowInterval($partInterval, $workflows, $startWorflow);
}

$input = explode("\n", trim(file_get_contents("input.txt")));
echo solve($input) . PHP_EOL;
