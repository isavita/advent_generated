
<?php

function generate_secret_number(int $initial_secret): int {
    $secret_number = $initial_secret;
    $prune_mod = 16777216;

    for ($i = 0; $i < 2000; $i++) {
        // Step 1
        $val_1 = $secret_number * 64;
        $secret_number = $secret_number ^ $val_1;
        $secret_number = $secret_number % $prune_mod;

        // Step 2
        $val_2 = floor($secret_number / 32);
        $secret_number = $secret_number ^ $val_2;
        $secret_number = $secret_number % $prune_mod;

        // Step 3
        $val_3 = $secret_number * 2048;
        $secret_number = $secret_number ^ $val_3;
        $secret_number = $secret_number % $prune_mod;
    }
    return $secret_number;
}

$file = fopen("input.txt", "r");
if ($file) {
    $total_sum = 0;
    while (($line = fgets($file)) !== false) {
        $initial_secret = intval(trim($line));
        $twentieth_secret = generate_secret_number($initial_secret);
        $total_sum += $twentieth_secret;
    }
    fclose($file);
    echo $total_sum . "\n";
} else {
    // Handle file open error if needed, but for challenge assume file exists and is readable
    // error_log("Error opening input.txt");
}

?>
