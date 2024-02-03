
<?php

class Node {
    public $name;
    public $children = [];
    public $parent;
}

function findOrCreateNode($name, &$nodes) {
    if (isset($nodes[$name])) {
        return $nodes[$name];
    }
    $node = new Node();
    $node->name = $name;
    $nodes[$name] = $node;
    return $node;
}

function buildOrbitMap($lines) {
    $nodes = [];
    foreach ($lines as $line) {
        $parts = explode(")", $line);
        $center = findOrCreateNode($parts[0], $nodes);
        $orbiter = findOrCreateNode($parts[1], $nodes);
        $center->children[] = $orbiter;
        $orbiter->parent = $center;
    }
    return $nodes;
}

function pathToRoot($node) {
    $path = [];
    while ($node != null) {
        $path[] = $node;
        $node = $node->parent;
    }
    return $path;
}

function findCommonAncestor($node1, $node2) {
    $path1 = pathToRoot($node1);
    $path2 = pathToRoot($node2);

    $i = count($path1) - 1;
    $j = count($path2) - 1;

    while ($i >= 0 && $j >= 0 && $path1[$i] === $path2[$j]) {
        $i--;
        $j--;
    }
    return $i + 1 + $j + 1;
}

$input = file("input.txt", FILE_IGNORE_NEW_LINES);
$orbitMap = buildOrbitMap($input);

$transfers = findCommonAncestor($orbitMap["YOU"]->parent, $orbitMap["SAN"]->parent);
echo $transfers . "\n";
?>
