
my @grid = 'input.txt'.IO.lines;
my $h = @grid.elems;
my $w = @grid[0].chars;

my %freq;
for ^$h X ^$w -> ($y,$x) {
    my $c = @grid[$y].substr($x,1);
    %freq{$c}.push: ($y,$x) if $c ne '.';
}

my %antinodes;
for %freq.kv -> $c, @coords {
    next unless @coords;
    for @coords.kv -> $i, ($ay,$ax) {
        for @coords[$i^..*] -> ($by,$bx) {
            my ($dy,$dx) = ($ay - $by, $ax - $bx);
            my @p = ($ay + $dy, $ax + $dx), ($by - $dy, $bx - $dx);
            for @p -> ($y,$x) {
                %antinodes{"$y,$x"} = 1
                  if 0 <= $y < $h && 0 <= $x < $w;
            }
        }
    }
}

say %antinodes.elems;
