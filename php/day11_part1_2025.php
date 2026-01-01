
<?php
$graph = [];
foreach (file('input.txt', FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES) as $line) {
    $parts = explode(':', $line, 2);
    if (count($parts) < 2) continue;
    $src = trim($parts[0]);
    $tokens = preg_split('/\s+/', trim($parts[1]), -1, PREG_SPLIT_NO_EMPTY);
    foreach ($tokens as $tok) {
        $graph[$src][] = $tok;
        if (!isset($graph[$tok])) $graph[$tok] = [];
    }
    if (!isset($graph[$src])) $graph[$src] = [];
}
function dfs($u, $t, &$g, &$m) {
    if ($u === $t) return 1;
    if (isset($m[$u])) return $m[$u];
    $sum = 0;
    foreach ($g[$u] ?? [] as $v) $sum += dfs($v, $t, $g, $m);
    return $m[$u] = $sum;
}
$memo = [];
$start = 'you';
$end   = 'out';
echo dfs($start, $end, $graph, $memo) . PHP_EOL;
