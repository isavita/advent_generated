
sub MAIN() {
    my @lines = 'input.txt'.IO.lines.map(*.chomp);
    my $width = @lines[0].chars;
    my $tree-count = 0;

    for 0 ..^ @lines.elems -> $y {
        my $x-pos = $y * 3;
        if @lines[$y].substr($x-pos % $width, 1) eq '#' {
            $tree-count++;
        }
    }
    say $tree-count;
}
