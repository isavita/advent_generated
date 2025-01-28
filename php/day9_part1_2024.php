
<?php

function solve() {
    $input = file_get_contents("input.txt");
    $disk_map_str = trim($input);

    $disk_blocks = parseDiskMap($disk_map_str);
    $compacted_disk_blocks = compactDisk($disk_blocks);
    $checksum = calculateChecksum($compacted_disk_blocks);

    echo $checksum . "\n";
}

function parseDiskMap($disk_map_str) {
    $disk_blocks = [];
    $file_id = 0;
    $is_file_length = true;
    $lengths = str_split($disk_map_str);

    foreach ($lengths as $length_char) {
        $length = intval($length_char);
        if ($is_file_length) {
            for ($i = 0; $i < $length; ++$i) {
                $disk_blocks[] = strval($file_id);
            }
            $file_id++;
        } else {
            for ($i = 0; $i < $length; ++$i) {
                $disk_blocks[] = '.';
            }
        }
        $is_file_length = !$is_file_length;
    }
    return $disk_blocks;
}

function compactDisk($disk_blocks) {
    while (true) {
        $first_free_index = -1;
        for ($i = 0; $i < count($disk_blocks); ++$i) {
            if ($disk_blocks[$i] === '.') {
                $first_free_index = $i;
                break;
            }
        }

        if ($first_free_index === -1) {
            break; // No more free space
        }

        $last_file_index = -1;
        for ($i = count($disk_blocks) - 1; $i >= 0; --$i) {
            if ($disk_blocks[$i] !== '.') {
                $last_file_index = $i;
                break;
            }
        }

        if ($last_file_index === -1 || $last_file_index <= $first_free_index) {
            break; // No more file blocks to move to the left free space
        }

        $disk_blocks[$first_free_index] = $disk_blocks[$last_file_index];
        $disk_blocks[$last_file_index] = '.';
    }
    return $disk_blocks;
}

function calculateChecksum($disk_blocks) {
    $checksum = 0;
    for ($i = 0; $i < count($disk_blocks); ++$i) {
        if ($disk_blocks[$i] !== '.') {
            $checksum += $i * intval($disk_blocks[$i]);
        }
    }
    return $checksum;
}

solve();

?>
