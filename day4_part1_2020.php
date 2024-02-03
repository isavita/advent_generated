
<?php

$input = file_get_contents("input.txt");
$passports = explode("\n\n", $input);

$validPassports = 0;
$requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"];

foreach ($passports as $passport) {
    if (isValid($passport, $requiredFields)) {
        $validPassports++;
    }
}

echo $validPassports . PHP_EOL;

function isValid($passport, $requiredFields) {
    foreach ($requiredFields as $field) {
        if (strpos($passport, $field . ":") === false) {
            return false;
        }
    }
    return true;
}
?>
