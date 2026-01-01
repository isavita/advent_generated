
<?php

/**
 * Disjoint Set Union (DSU) class to manage connected components (circuits).
 * This implementation uses path compression and union-by-size for optimal performance.
 */
class DisjointSet {
    private array $parent;
    private array $size;

    /**
     * Initializes the DSU structure with $n individual sets.
     */
    public function __construct(int $n) {
        $this->parent = range(0, $n - 1);
        $this->size = array_fill(0, $n, 1);
    }

    /**
     * Find the root representative of the set containing element $i.
     * Implements path compression to flatten the structure on lookup.
     */
    public function find(int $i): int {
        if ($this->parent[$i] === $i) {
            return $i;
        }
        return $this->parent[$i] = $this->find($this->parent[$i]);
    }

    /**
     * Merges the sets containing elements $i and $j.
     * Uses union-by-size to maintain a balanced tree.
     */
    public function union(int $i, int $j): void {
        $rootI = $this->find($i);
        $rootJ = $this->find($j);
        if ($rootI !== $rootJ) {
            if ($this->size[$rootI] < $this->size[$rootJ]) {
                [$rootI, $rootJ] = [$rootJ, $rootI];
            }
            $this->parent[$rootJ] = $rootI;
            $this->size[$rootI] += $this->size[$rootJ];
        }
    }

    /**
     * Returns an array containing the sizes of all current disjoint sets.
     */
    public function getSizes(): array {
        $sizes = [];
        foreach ($this->parent as $i => $p) {
            if ($p === $i) {
                $sizes[] = $this->size[$i];
            }
        }
        return $sizes;
    }
}

/**
 * Main function to solve the playground junction box challenge.
 * Reads coordinates from input.txt and calculates the product of the three largest circuits
 * formed after 1,000 closest-pair connections.
 */
function main(): void {
    $filename = 'input.txt';
    if (!file_exists($filename)) {
        return;
    }

    // Parse coordinates into an array of points [x, y, z].
    $lines = file($filename, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
    if ($lines === false) return;

    $points = [];
    foreach ($lines as $line) {
        $coords = explode(',', trim($line));
        if (count($coords) === 3) {
            $points[] = array_map('intval', $coords);
        }
    }

    $numPoints = count($points);
    if ($numPoints === 0) return;

    /**
     * Identify the 1000 closest pairs using a Max-Heap (SplPriorityQueue).
     * By maintaining a heap of size 1,000 and always discarding the largest distance 
     * in that heap when a smaller one is found, we efficiently gather the 
     * 1,000 globally shortest distances.
     */
    $heap = new SplPriorityQueue();
    $heap->setExtractFlags(SplPriorityQueue::EXTR_BOTH);

    for ($i = 0; $i < $numPoints; $i++) {
        $x1 = $points[$i][0];
        $y1 = $points[$i][1];
        $z1 = $points[$i][2];
        for ($j = $i + 1; $j < $numPoints; $j++) {
            $dx = $x1 - $points[$j][0];
            $dy = $y1 - $points[$j][1];
            $dz = $z1 - $points[$j][2];
            
            // Euclidean distance squared is sufficient for comparison and faster than sqrt.
            $distSq = ($dx * $dx) + ($dy * $dy) + ($dz * $dz);

            if ($heap->count() < 1000) {
                $heap->insert([$i, $j], $distSq);
            } else {
                $maxNode = $heap->top();
                if ($distSq < $maxNode['priority']) {
                    $heap->extract();
                    $heap->insert([$i, $j], $distSq);
                }
            }
        }
    }

    /**
     * Connect the 1000 pairs identified above.
     * The final circuit sizes are independent of the union order.
     */
    $dsu = new DisjointSet($numPoints);
    while (!$heap->isEmpty()) {
        $item = $heap->extract();
        $dsu->union($item['data'][0], $item['data'][1]);
    }

    /**
     * Extract the resulting circuit sizes, sort them descending,
     * and calculate the product of the top 3.
     */
    $sizes = $dsu->getSizes();
    rsort($sizes);

    $product = 1;
    $countToMultiply = min(3, count($sizes));
    for ($k = 0; $k < $countToMultiply; $k++) {
        $product *= $sizes[$k];
    }

    // Output the final result to standard output.
    echo $product . PHP_EOL;
}

// Entry point of the program.
main();
