
<?php
function readInput()
{
    $f = fopen('input.txt', 'r');
    if (!$f) exit(1);
    $pts = [];
    $xs = [];
    $ys = [];
    while (($line = fgets($f)) !== false) {
        if (sscanf($line, " %d , %d ", $x, $y) !== 2) continue;
        $pts[] = [$x, $y];
        $xs[] = $x;
        $ys[] = $y;
    }
    fclose($f);
    return [$pts, $xs, $ys];
}
list($pts, $xs, $ys) = readInput();
$n = count($pts);
if ($n === 0) {
    echo "Largest valid area: 0\n";
    exit;
}
sort($xs, SORT_NUMERIC);
sort($ys, SORT_NUMERIC);
$ux = [];
$uy = [];
$prev = null;
foreach ($xs as $v) {
    if ($v !== $prev) {$ux[] = $v; $prev = $v;}
}
$prev = null;
foreach ($ys as $v) {
    if ($v !== $prev) {$uy[] = $v; $prev = $v;}
}
$xidx = [];
$yidx = [];
foreach ($ux as $i => $v) $xidx[$v] = $i;
foreach ($uy as $i => $v) $yidx[$v] = $i;
$W = 2 * count($ux) + 1;
$H = 2 * count($uy) + 1;
$colW = array_fill(0, $W, 0);
$rowH = array_fill(0, $H, 0);
$colW[0] = 1;
for ($i = 0; $i < count($ux); $i++) {
    $colW[2*$i+1] = 1;
    $colW[2*$i+2] = ($i+1 < count($ux)) ? max(0, $ux[$i+1] - $ux[$i] - 1) : 1;
}
$rowH[0] = 1;
for ($i = 0; $i < count($uy); $i++) {
    $rowH[2*$i+1] = 1;
    $rowH[2*$i+2] = ($i+1 < count($uy)) ? max(0, $uy[$i+1] - $uy[$i] - 1) : 1;
}
$grid = [];
for ($y = 0; $y < $H; $y++) $grid[$y] = array_fill(0, $W, 0);
foreach ($pts as $k => $p) {
    $a = $p;
    $b = $pts[($k+1)%$n];
    $gx1 = 2*$xidx[$a[0]]+1; $gy1 = 2*$yidx[$a[1]]+1;
    $gx2 = 2*$xidx[$b[0]]+1; $gy2 = 2*$yidx[$b[1]]+1;
    if ($gx1 === $gx2) {
        $y0 = min($gy1,$gy2); $y1 = max($gy1,$gy2);
        for ($y=$y0;$y<=$y1;$y++) if ($rowH[$y]>0) $grid[$y][$gx1]=1;
    } else {
        $x0 = min($gx1,$gx2); $x1 = max($gx1,$gx2);
        for ($x=$x0;$x<=$x1;$x++) if ($colW[$x]>0) $grid[$gy1][$x]=1;
    }
}
$queue = new SplQueue();
$queue->enqueue([0,0]);
$grid[0][0]=2;
$dirs = [[0,1],[0,-1],[1,0],[-1,0]];
while (!$queue->isEmpty()) {
    [$cx,$cy] = $queue->dequeue();
    foreach ($dirs as $d) {
        $nx=$cx+$d[0]; $ny=$cy+$d[1];
        if ($nx>=0 && $nx<$W && $ny>=0 && $ny<$H && $grid[$ny][$nx]==0) {
            $grid[$ny][$nx]=2;
            $queue->enqueue([$nx,$ny]);
        }
    }
}
$P = [];
for ($y=0;$y<$H;$y++) {
    $P[$y]=array_fill(0,$W,0);
    for ($x=0;$x<$W;$x++) {
        $val = ($grid[$y][$x]!=2) ? $colW[$x]*$rowH[$y] : 0;
        $left = $x? $P[$y][$x-1]:0;
        $up   = $y? $P[$y-1][$x]:0;
        $diag = ($x && $y)? $P[$y-1][$x-1]:0;
        $P[$y][$x] = $val + $left + $up - $diag;
    }
}
$maxArea = 0;
for ($i=0;$i<$n;$i++) {
    for ($j=$i;$j<$n;$j++) {
        $a=$pts[$i]; $b=$pts[$j];
        $w = abs($a[0]-$b[0]) + 1;
        $h = abs($a[1]-$b[1]) + 1;
        $area = $w*$h;
        if ($area <= $maxArea) continue;
        $gx1 = 2*$xidx[$a[0]]+1; $gy1 = 2*$yidx[$a[1]]+1;
        $gx2 = 2*$xidx[$b[0]]+1; $gy2 = 2*$yidx[$b[1]]+1;
        if ($gx1>$gx2){$t=$gx1;$gx1=$gx2;$gx2=$t;}
        if ($gy1>$gy2){$t=$gy1;$gy1=$gy2;$gy2=$t;}
        $total = $P[$gy2][$gx2];
        $left  = $gx1? $P[$gy2][$gx1-1]:0;
        $up    = $gy1? $P[$gy1-1][$gx2]:0;
        $diag  = ($gx1 && $gy1)? $P[$gy1-1][$gx1-1]:0;
        $valid = $total - $left - $up + $diag;
        if ($valid == $area) $maxArea = $area;
    }
}
echo "Largest valid area: $maxArea\n";
