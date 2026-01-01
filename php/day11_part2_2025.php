
<?php
$nodes = [];
$adj = [];
$nameToIdx = [];

function idx(string $s): int {
    global $nodes, $adj, $nameToIdx;
    if (isset($nameToIdx[$s])) {
        return $nameToIdx[$s];
    }
    $i = count($nodes);
    $nameToIdx[$s] = $i;
    $nodes[$i] = $s;
    $adj[$i] = [];
    return $i;
}

function add_edge(int $u, int $v): void {
    global $adj;
    $adj[$u][] = $v;
}

function dfs(int $cur, int $tgt, array &$memo): int {
    if ($cur === $tgt) {
        return 1;
    }
    if (isset($memo[$cur])) {
        return $memo[$cur];
    }
    $sum = 0;
    foreach ($GLOBALS['adj'][$cur] as $next) {
        $sum += dfs($next, $tgt, $memo);
    }
    $memo[$cur] = $sum;
    return $sum;
}

function count_paths(int $s, int $t): int {
    $memo = [];
    return dfs($s, $t, $memo);
}

$handle = fopen('input.txt', 'r');
if ($handle) {
    while (($line = fgets($handle)) !== false) {
        $line = rtrim($line, "\r\n");
        $line = ltrim($line);
        if ($line === '') continue;
        $colonPos = strpos($line, ':');
        if ($colonPos === false) continue;
        $src = substr($line, 0, $colonPos);
        $dstPart = substr($line, $colonPos + 1);
        $u = idx($src);
        $tokens = preg_split('/[\s\t]+/', trim($dstPart));
        foreach ($tokens as $tok) {
            if ($tok === '') continue;
            $v = idx($tok);
            add_edge($u, $v);
        }
    }
    fclose($handle);
}

$svr = idx('svr');
$dac = idx('dac');
$fft = idx('fft');
$out = idx('out');

$s1 = count_paths($svr, $dac) * count_paths($dac, $fft) * count_paths($fft, $out);
$s2 = count_paths($svr, $fft) * count_paths($fft, $dac) * count_paths($dac, $out);
$total = $s1 + $s2;

echo "Paths (svr->dac->fft->out): $s1\n";
echo "Paths (svr->fft->dac->out): $s2\n";
echo "Total paths visiting both: $total\n";
?>
