
sub MAIN() {
    my @lengths = 'input.txt'.IO.slurp.trim.split(',').map(*.Int);

    my @list = ^256;
    my $currentPosition = 0;
    my $skipSize = 0;

    for @lengths -> $length {
        for ^($length div 2) -> $i {
            my $start = ($currentPosition + $i) % 256;
            my $end = ($currentPosition + $length - 1 - $i) % 256;
            (@list[$start], @list[$end]) = (@list[$end], @list[$start]);
        }

        $currentPosition = ($currentPosition + $length + $skipSize) % 256;
        $skipSize++;
    }

    say @list[0] * @list[1];
}
