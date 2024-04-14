<?php

function validateYear($value, $min, $max) {
    return is_numeric($value) && $value >= $min && $value <= $max;
}

function validateHgt($value) {
    if (substr($value, -2) === "cm") {
        $hgt = intval(substr($value, 0, -2));
        return $hgt >= 150 && $hgt <= 193;
    } elseif (substr($value, -2) === "in") {
        $hgt = intval(substr($value, 0, -2));
        return $hgt >= 59 && $hgt <= 76;
    }
    return false;
}

function validateHcl($value) {
    return preg_match('/^#[0-9a-f]{6}$/', $value);
}

function validateEcl($value) {
    $validEcl = ['amb' => true, 'blu' => true, 'brn' => true, 'gry' => true, 'grn' => true, 'hzl' => true, 'oth' => true];
    return isset($validEcl[$value]);
}

function validatePid($value) {
    return preg_match('/^[0-9]{9}$/', $value);
}

function isValidPassport($passport) {
    $fields = explode(" ", $passport);
    $fieldMap = [];
    foreach ($fields as $field) {
        list($key, $val) = explode(":", $field);
        $fieldMap[$key] = $val;
    }

    return validateYear($fieldMap['byr'] ?? '', 1920, 2002) &&
           validateYear($fieldMap['iyr'] ?? '', 2010, 2020) &&
           validateYear($fieldMap['eyr'] ?? '', 2020, 2030) &&
           validateHgt($fieldMap['hgt'] ?? '') &&
           validateHcl($fieldMap['hcl'] ?? '') &&
           validateEcl($fieldMap['ecl'] ?? '') &&
           validatePid($fieldMap['pid'] ?? '');
}

$passports = file_get_contents("input.txt");
$passports = preg_split('/\n\s*\n/', $passports);
$validPassports = 0;

foreach ($passports as $passport) {
    $passport = str_replace("\n", " ", $passport);
    $passport = trim($passport);
    if (isValidPassport($passport)) {
        $validPassports++;
    }
}

echo $validPassports;

?>