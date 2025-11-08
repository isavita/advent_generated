
sub knot-hash(Str $input) {
    my @lengths = $input.comb».ord;
    @lengths.append: 17, 31, 73, 47, 23;
    my $size = 256;
    my @list = ^$size;
    my ($pos, $skip) = 0, 0;

    for ^64 {
        for @lengths -> $len {
            my @tmp = @list.rotate($pos);
            @tmp[^$len] = @tmp[^$len].reverse;
            @list = @tmp.rotate(-$pos);
            $pos = ($pos + $len + $skip++) % $size;
        }
    }

    (^16).map(-> $i { [+^] @list[$i*16..$i*16+15] })
          .map(-> $n { sprintf '%02x', $n }).join
}

sub MAIN {
    my $input = 'input.txt'.IO.slurp.trim;
    say knot-hash($input);
}
