<?php

class File {
    public $size;
}

class FileDirectory {
    public $files = [];
    public $directories = [];

    public function totalSize() {
        $size = 0;
        foreach ($this->files as $file) {
            $size += $file->size;
        }
        foreach ($this->directories as $dir) {
            $size += $dir->totalSize();
        }
        return $size;
    }
}

function newFileDirectory() {
    return new FileDirectory();
}

$root = newFileDirectory();
$currentDir = $root;
$directoryStack = [$root];

$file = fopen("input.txt", "r");
if ($file === false) {
    die("Unable to open file.");
}

while (($line = fgets($file)) !== false) {
    $line = trim($line);
    if (strpos($line, "$ cd") === 0) {
        $path = trim(substr($line, 4));
        if ($path === "/") {
            $currentDir = $root;
            $directoryStack = [$root];
        } elseif ($path === "..") {
            array_pop($directoryStack);
            $currentDir = end($directoryStack);
        } else {
            if (!isset($currentDir->directories[$path])) {
                $currentDir->directories[$path] = newFileDirectory();
            }
            $currentDir = $currentDir->directories[$path];
            $directoryStack[] = $currentDir;
        }
    } elseif (strpos($line, "dir") === 0) {
        $dirName = trim(substr($line, 4));
        $currentDir->directories[$dirName] = newFileDirectory();
    } else {
        $parts = explode(" ", $line);
        if (count($parts) == 2) {
            $size = intval($parts[0]);
            $fileName = $parts[1];
            $currentDir->files[$fileName] = new File();
            $currentDir->files[$fileName]->size = $size;
        }
    }
}
fclose($file);

$sumSizes = 0;
function calculateSizes($dir) {
    global $sumSizes;
    $dirSize = $dir->totalSize();
    if ($dirSize <= 100000) {
        $sumSizes += $dirSize;
    }
    foreach ($dir->directories as $subDir) {
        calculateSizes($subDir);
    }
}
calculateSizes($root);

echo $sumSizes . "\n";