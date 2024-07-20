
<?php

function readInput($filename) {
    return file($filename, FILE_IGNORE_NEW_LINES);
}

function simulateVirusCarrier($grid, $bursts) {
    // Directions: up, right, down, left
    $directions = [
        'up' => [-1, 0],
        'right' => [0, 1],
        'down' => [1, 0],
        'left' => [0, -1]
    ];
    
    // Initial position (center of the grid)
    $x = floor(count($grid) / 2);
    $y = floor(strlen($grid[0]) / 2);
    $facing = 'up';
    
    // Convert grid to a more manageable structure
    $infected = [];
    foreach ($grid as $i => $line) {
        for ($j = 0; $j < strlen($line); $j++) {
            if ($line[$j] === '#') {
                $infected["$i,$j"] = true;
            }
        }
    }
    
    $infectionCount = 0;

    for ($i = 0; $i < $bursts; $i++) {
        $currentPos = "$x,$y";
        
        // Determine the direction and infection status
        if (isset($infected[$currentPos])) {
            // Current node is infected
            $facing = turnRight($facing);
            unset($infected[$currentPos]); // Clean the node
        } else {
            // Current node is clean
            $facing = turnLeft($facing);
            $infected[$currentPos] = true; // Infect the node
            $infectionCount++;
        }
        
        // Move forward in the current direction
        $x += $directions[$facing][0];
        $y += $directions[$facing][1];
    }
    
    return $infectionCount;
}

function turnLeft($facing) {
    switch ($facing) {
        case 'up': return 'left';
        case 'left': return 'down';
        case 'down': return 'right';
        case 'right': return 'up';
    }
}

function turnRight($facing) {
    switch ($facing) {
        case 'up': return 'right';
        case 'right': return 'down';
        case 'down': return 'left';
        case 'left': return 'up';
    }
}

// Main execution
$inputFile = 'input.txt';
$grid = readInput($inputFile);
$bursts = 10000;
$infectionCount = simulateVirusCarrier($grid, $bursts);

echo "Number of bursts causing infection: $infectionCount\n";
