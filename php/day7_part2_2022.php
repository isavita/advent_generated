
<?php

function parseInput($filename) {
    $lines = file($filename, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
    $filesystem = [];
    $currentPath = ['/'];

    foreach ($lines as $line) {
        if (strpos($line, '$') === 0) {
            // Command line
            $command = trim(substr($line, 2));
            if (preg_match('/^cd (.+)$/', $command, $matches)) {
                $dir = $matches[1];
                if ($dir === '..') {
                    array_pop($currentPath);
                } elseif ($dir === '/') {
                    $currentPath = ['/'];
                } else {
                    $currentPath[] = $dir;
                }
            }
        } else {
            // Output from ls
            $parts = explode(' ', $line);
            if (count($parts) === 2) {
                if ($parts[0] === 'dir') {
                    // Directory, do nothing for size
                } else {
                    // File
                    $size = (int)$parts[0];
                    $path = implode('/', $currentPath);
                    if (!isset($filesystem[$path])) {
                        $filesystem[$path] = [];
                    }
                    $filesystem[$path][] = $size;
                }
            }
        }
    }
    return $filesystem;
}

function calculateDirectorySizes($filesystem) {
    $sizes = [];
    foreach ($filesystem as $path => $files) {
        $totalSize = array_sum($files);
        $sizes[$path] = $totalSize;
    }

    // Calculate sizes for parent directories
    foreach ($sizes as $path => $size) {
        $parts = explode('/', $path);
        for ($i = count($parts) - 1; $i > 0; $i--) {
            $parentPath = implode('/', array_slice($parts, 0, $i));
            if (isset($sizes[$parentPath])) {
                $sizes[$parentPath] += $size;
            } else {
                $sizes[$parentPath] = $size;
            }
        }
    }
    return $sizes;
}

function findSmallDirectories($sizes, $limit) {
    $total = 0;
    foreach ($sizes as $size) {
        if ($size <= $limit) {
            $total += $size;
        }
    }
    return $total;
}

function findSmallestDirectoryToDelete($sizes, $neededSpace) {
    $totalUsedSpace = max($sizes);
    $totalDiskSpace = 70000000;
    $currentUnusedSpace = $totalDiskSpace - $totalUsedSpace;
    $requiredSpace = 30000000;
    $spaceToFree = $requiredSpace - $currentUnusedSpace;

    $smallestSize = PHP_INT_MAX;
    foreach ($sizes as $size) {
        if ($size >= $spaceToFree && $size < $smallestSize) {
            $smallestSize = $size;
        }
    }
    return $smallestSize;
}

// Main execution
$filesystem = parseInput('input.txt');
$sizes = calculateDirectorySizes($filesystem);
$sumOfSmallDirectories = findSmallDirectories($sizes, 100000);
$smallestDirectoryToDelete = findSmallestDirectoryToDelete($sizes, 30000000);

echo "Sum of total sizes of directories with size at most 100000: $sumOfSmallDirectories\n";
echo "Size of the smallest directory to delete: $smallestDirectoryToDelete\n";
?>
