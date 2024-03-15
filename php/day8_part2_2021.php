<?php

function jumbledSevenSegment($input) {
    $parsedInput = [];
    $lines = explode("\n", trim($input));
    foreach ($lines as $line) {
        $parts = preg_match_all('/([a-g]+)/', $line, $matches);
        if (count($matches[1]) != 14) {
            die("should be 14 parts in each input line, got " . count($matches[1]));
        }
        $fourteen = array_map('alphabetizeString', $matches[1]);
        $parsedInput[] = $fourteen;
    }

    $ans = 0;
    $indexToCharacters = array_fill(0, 10, '');
    foreach ($parsedInput as $set) {
        $workingSet = array_slice($set, 0, 10);
        $killIndices = [];

        foreach ($workingSet as $i => $mapping) {
            switch (strlen($mapping)) {
                case 2:
                    $indexToCharacters[1] = $mapping;
                    $killIndices[] = $i;
                    break;
                case 4:
                    $indexToCharacters[4] = $mapping;
                    $killIndices[] = $i;
                    break;
                case 3:
                    $indexToCharacters[7] = $mapping;
                    $killIndices[] = $i;
                    break;
                case 7:
                    $indexToCharacters[8] = $mapping;
                    $killIndices[] = $i;
                    break;
            }
        }

        $workingSet = removeSliceIndices($workingSet, $killIndices);

        $zeroThreeOrNine = [];
        $killIndices = [];
        foreach ($workingSet as $i => $mapping) {
            if (checkStringOverlap($mapping, $indexToCharacters[1])) {
                $zeroThreeOrNine[] = $mapping;
                $killIndices[] = $i;
            }
        }
        if (count($zeroThreeOrNine) != 3) {
            die("one three or nine does not have three matches: got " . count($zeroThreeOrNine));
        }

        foreach ($zeroThreeOrNine as $i => $maybe039) {
            if (strlen($maybe039) == 5) {
                $indexToCharacters[3] = $maybe039;
                $zeroThreeOrNine = removeSliceIndices($zeroThreeOrNine, array($i));
                break;
            }
        }

        foreach ($zeroThreeOrNine as $i => $maybe09) {
            if (checkStringOverlap($maybe09, $indexToCharacters[4])) {
                $indexToCharacters[9] = $maybe09;
                $zeroThreeOrNine = removeSliceIndices($zeroThreeOrNine, array($i));
            }
        }

        $indexToCharacters[0] = $zeroThreeOrNine[0];

        $workingSet = removeSliceIndices($workingSet, $killIndices);
        if (count($workingSet) != 3) {
            die("expected length of 3 at this stage, got " . count($workingSet));
        }

        foreach ($workingSet as $i => $mapping) {
            if (strlen($mapping) == 6) {
                $indexToCharacters[6] = $mapping;
                $workingSet = removeSliceIndices($workingSet, array($i));
            }
        }

        foreach ($workingSet as $i => $mapping) {
            if (checkStringOverlap($indexToCharacters[9], $mapping)) {
                $indexToCharacters[5] = $mapping;
                $workingSet = removeSliceIndices($workingSet, array($i));
            }
        }

        if (count($workingSet) != 1) {
            die("expected length of 1 at this stage, got " . count($workingSet));
        }

        $indexToCharacters[2] = $workingSet[0];

        $num = 0;
        foreach (array_slice($set, 10) as $out) {
            foreach ($indexToCharacters as $i => $mapping) {
                if ($out == $mapping) {
                    $num = $num * 10 + $i;
                }
            }
        }
        $ans += $num;
    }

    return $ans;
}

function removeSliceIndices($sli, $indices) {
    $ans = [];
    foreach ($sli as $i => $v) {
        if (!in_array($i, $indices)) {
            $ans[] = $v;
        }
    }
    return $ans;
}

function checkStringOverlap($larger, $smaller) {
    if (strlen($larger) < strlen($smaller)) {
        list($larger, $smaller) = array($smaller, $larger);
    }

    $largeMap = array_flip(str_split($larger));
    foreach (str_split($smaller) as $r) {
        if (!isset($largeMap[$r])) {
            return false;
        }
    }
    return true;
}

function alphabetizeString($input) {
    $chars = str_split($input);
    sort($chars);
    return implode('', $chars);
}

$input = file_get_contents('input.txt');
$ans = jumbledSevenSegment($input);
echo $ans . "\n";