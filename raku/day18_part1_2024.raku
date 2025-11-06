
my constant \size = 71;
my @grid = [False xx size] xx size;

slurp('input.txt').lines[^1024].map: -> $ln {
    my ($x,$y) = $ln.split(',').map: *.Int;
    @grid[$y][$x] = True if 0 <= $x < size and 0 <= $y < size;
}

my @dirs = (1,0), (-1,0), (0,1), (0,-1);
my @visited = [False xx size] xx size;
my @q = (0,0,0);
@visited[0][0] = True;

while @q {
    my ($x,$y,$s) = @q.shift, @q.shift, @q.shift;
    if $x == size-1 and $y == size-1 {
        say $s;
        exit 0;
    }
    for @dirs -> ($dx,$dy) {
        my ($nx,$ny) = $x+$dx, $y+$dy;
        if 0 <= $nx < size and 0 <= $ny < size
           and !@grid[$ny][$nx] and !@visited[$ny][$nx] {
            @visited[$ny][$nx] = True;
            @q.append: $nx,$ny,$s+1;
        }
    }
}
say 'No path';
