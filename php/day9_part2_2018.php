<?php

class Node {
    public $value;
    public $prev;
    public $next;

    public function __construct($value) {
        $this->value = $value;
        $this->prev = null;
        $this->next = null;
    }
}

// Increase memory limit if possible
ini_set('memory_limit', '5120M');

$input = trim(file_get_contents('input.txt'));
$data = preg_split('/\s+/', $input);

$players = (int)$data[0];
$last_marble = (int)$data[6] * 100;

$scores = array_fill(0, $players, 0);
$current_player = 0;

// Initialize circular linked list
$current = new Node(0);
$current->prev = $current;
$current->next = $current;

for ($marble = 1; $marble <= $last_marble; $marble++) {
    if ($marble % 23 === 0) {
        // Move 7 nodes back
        for ($i = 0; $i < 7; $i++) {
            $current = $current->prev;
        }
        
        // Update score
        $scores[$current_player] += $marble + $current->value;
        
        // Remove node and clear references
        $prev_node = $current->prev;
        $next_node = $current->next;
        $prev_node->next = $next_node;
        $next_node->prev = $prev_node;
        
        // Clear references to help GC
        $removed = $current;
        $current = $next_node;
        $removed->prev = null;
        $removed->next = null;
        unset($removed);
    } else {
        // Insert new marble
        $current = $current->next;
        $new_node = new Node($marble);
        
        $new_node->prev = $current;
        $new_node->next = $current->next;
        $current->next->prev = $new_node;
        $current->next = $new_node;
        $current = $new_node;
    }
    
    $current_player = ($current_player + 1) % $players;
    
    // Periodically trigger garbage collection
    if ($marble % 1000 === 0) {
        gc_collect_cycles();
    }
}

echo max($scores) . "\n";
?>
