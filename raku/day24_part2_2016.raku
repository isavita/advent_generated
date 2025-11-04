
my @grid = 'input.txt'.IO.lines;
my $rows = @grid.elems;
my $cols = @grid[0].chars;

my @poi;
for ^$rows X ^$cols -> ($r,$c) {
    my $ch = @grid[$r].substr($c,1);
    if $ch ~~ /\d/ { @poi.push: [$r,$c,$ch] }
}
my $poi-count = @poi.elems;

my @dist;
@dist.push: [Inf xx $poi-count] for ^$poi-count;

for @poi -> ($r,$c,$id) {
    my $idx = $id - '0';
    my @queue = ($r,$c,0);
    my @visited;
    @visited[$r][$c] = True;
    my @dr = 0,0,1,-1;
    my @dc = 1,-1,0,0;
    while @queue {
        my ($y,$x,$d) = @queue.shift, @queue.shift, @queue.shift;
        if @grid[$y].substr($x,1) ~~ /\d/ {
            my $t = @grid[$y].substr($x,1) - '0';
            @dist[$idx][$t] = $d;
        }
        for ^4 -> $i {
            my ($ny,$nx) = $y+@dr[$i], $x+@dc[$i];
            next unless 0 <= $ny < $rows and 0 <= $nx < $cols;
            next if @grid[$ny].substr($nx,1) eq '#';
            next if @visited[$ny][$nx];
            @visited[$ny][$nx] = True;
            @queue.append: ($ny,$nx,$d+1);
        }
    }
}

sub dfs($cur, @vis, $ret) {
    my $cnt = @vis.grep({$_}).elems;
    if $cnt == $poi-count {
        return $ret ?? @dist[$cur][0] !! 0;
    }
    my $best = Inf;
    for ^$poi-count -> $i {
        next if @vis[$i];
        @vis[$i] = True;
        $best min= @dist[$cur][$i] + dfs($i, @vis.clone, $ret);
        @vis[$i] = False;
    }
    $best
}

say dfs(0, [True, |(False xx $poi-count-1)], True);
