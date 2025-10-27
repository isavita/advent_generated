
use v6;

my @grid = 'input.txt'.IO.lines;
my $h = @grid.elems;
my $w = @grid[0].chars;

my ($S, $E);
for ^$h X ^$w -> ($i,$j) {
    $S = [$i,$j] if @grid[$i].substr($j,1) eq 'S';
    $E = [$i,$j] if @grid[$i].substr($j,1) eq 'E';
}

my @walls;
@walls[$_][^$w] = False for ^$h;
for ^$h X ^$w -> ($i,$j) {
    @walls[$i][$j] = True if @grid[$i].substr($j,1) eq '#';
}

my @track = gather for ^$h X ^$w -> ($i,$j) {
    take [$i,$j] unless @walls[$i][$j];
}

sub bfs($start) {
    my @d = -1 xx ($h*$w);
    my @q = $start;
    @d[$start[0]*$w + $start[1]] = 0;
    while @q {
        my ($x,$y) = @q.shift;
        for ([-1,0],[1,0],[0,-1],[0,1]) -> ($dx,$dy) {
            my ($nx,$ny) = $x+$dx, $y+$dy;
            next if $nx < 0 || $nx >= $h || $ny < 0 || $ny >= $w;
            next if @walls[$nx][$ny];
            my $idx = $nx*$w + $ny;
            next if @d[$idx] >= 0;
            @d[$idx] = @d[$x*$w + $y] + 1;
            @q.push([$nx,$ny]);
        }
    }
    @d
}

my @dS = bfs($S);
my @dE = bfs($E);
my $normal = @dS[$E[0]*$w + $E[1]];
put 0 and exit if $normal < 0;

my $cnt = 0;
for @track -> $p {
    my ($x,$y) = @$p;
    my $sd = @dS[$x*$w + $y];
    next if $sd < 0;
    for [-1,0],[1,0],[0,-1],[0,1] -> ($dx,$dy) {
        my ($m1x,$m1y) = $x+$dx, $y+$dy;
        next unless 0 <= $m1x < $h && 0 <= $m1y < $w;
        for [-1,0],[1,0],[0,-1],[0,1] -> ($dx2,$dy2) {
            my ($m2x,$m2y) = $m1x+$dx2, $m1y+$dy2;
            next unless 0 <= $m2x < $h && 0 <= $m2y < $w;
            next if @walls[$m2x][$m2y];
            my $ed = @dE[$m2x*$w + $m2y];
            next if $ed < 0;
            $cnt++ if $normal - ($sd + 2 + $ed) >= 100;
        }
    }
}
put $cnt;
