
my @DIRS = (0,-1, 0,1, -1,0, 1,0);
my %SLOPE = '^' => 0, 'v' => 1, '<' => 2, '>' => 3;

my @grid = 'input.txt'.IO.lines>>.comb;
my $h = @grid.elems;
my $w = @grid[0].elems;

my ($sx,$sy) = (0..^$w).first({ @grid[0][$_] eq '.' }, :k), 0;
my ($ex,$ey) = (0..^$w).first({ @grid[$h-1][$_] eq '.' }, :k), $h-1;

my @visited;
my $max = 0;

sub dfs($x,$y,$steps) {
    if $x == $ex && $y == $ey {
        $max max= $steps;
        return;
    }
    my $tile = @grid[$y][$x];
    my @todo = %SLOPE{$tile}:exists
        ?? (@DIRS[%SLOPE{$tile}*2], @DIRS[%SLOPE{$tile}*2+1]).flat
        !! @DIRS;
    for @todo -> $dx,$dy {
        my ($nx,$ny) = $x+$dx,$y+$dy;
        next unless 0 <= $nx < $w && 0 <= $ny < $h;
        next if @grid[$ny][$nx] eq '#' || @visited[$ny][$nx];
        @visited[$ny][$nx] = True;
        dfs($nx,$ny,$steps+1);
        @visited[$ny][$nx] = False;
    }
}
@visited[$sy][$sx] = True;
dfs($sx,$sy,0);
say $max;
