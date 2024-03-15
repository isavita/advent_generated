<?php

class Mirror {
    public $rows = [];
    public $cols = [];
}

function parseInput($input) {
    $mirrors = [];
    $mirrorStr = [];
    foreach ($input as $line) {
        if ($line == '') {
            $mirrors[] = parseMirror($mirrorStr);
            $mirrorStr = [];
        } else {
            $mirrorStr[] = $line;
        }
    }
    $mirrors[] = parseMirror($mirrorStr);
    return $mirrors;
}

function parseMirror($mirrorStr) {
    $mirror = new Mirror();
    $mirror->rows = array_fill(0, count($mirrorStr), 0);
    $mirror->cols = array_fill(0, strlen($mirrorStr[0]), 0);
    foreach ($mirrorStr as $y => $line) {
        for ($x = 0; $x < strlen($line); $x++) {
            $mirror->rows[$y] <<= 1;
            $mirror->cols[$x] <<= 1;
            if ($line[$x] == '#') {
                $mirror->rows[$y]++;
                $mirror->cols[$x]++;
            }
        }
    }
    return $mirror;
}

function getMirrorAxis($lines) {
    for ($i = 1; $i < count($lines); $i++) {
        $isMirror = true;
        for ($j = 0; $isMirror && $j < min($i, count($lines) - $i); $j++) {
            if ($lines[$i - 1 - $j] != $lines[$i + $j]) {
                $isMirror = false;
            }
        }
        if ($isMirror) {
            return $i;
        }
    }
    return 0;
}

function getMirrorAxisWithOneSmudge($lines) {
    for ($i = 1; $i < count($lines); $i++) {
        $isMirror = true;
        $numSmudges = 0;
        for ($j = 0; $isMirror && $j < min($i, count($lines) - $i); $j++) {
            if ($lines[$i - 1 - $j] != $lines[$i + $j]) {
                if ($numSmudges > 0) {
                    $isMirror = false;
                } else {
                    $dif = $lines[$i - 1 - $j] ^ $lines[$i + $j];
                    $isOnlyOneSmudge = ($dif & ($dif - 1)) == 0;
                    if ($isOnlyOneSmudge) {
                        $numSmudges++;
                    } else {
                        $isMirror = false;
                    }
                }
            }
        }
        if ($isMirror && $numSmudges == 1) {
            return $i;
        }
    }
    return 0;
}

function solve($input) {
    $mirrors = parseInput($input);
    $res = 0;
    foreach ($mirrors as $mirror) {
        $res += getMirrorAxis($mirror->cols);
        $res += getMirrorAxis($mirror->rows) * 100;
    }
    return $res;
}

function readInputFromFile($fileName) {
    $file = file_get_contents($fileName);
    return explode("\n", trim($file));
}

$input = readInputFromFile('input.txt');
echo solve($input);