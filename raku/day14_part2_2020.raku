
sub MAIN {
    my $mask = '';
    my %mem;

    for 'input.txt'.IO.lines {
        if /mask \s+ '=' \s+ (<[01X]>+)/ {
            $mask = $0.Str;
        }
        elsif /mem '[' (\d+) ']' \s+ '=' \s+ (\d+)/ {
            my $addr = $0.Int;
            my $val  = $1.Int;

            my @bits = $addr.fmt('%036b').comb;
            for ^36 -> $i {
                given $mask.substr($i,1) {
                    when '1' { @bits[$i] = '1' }
                    when 'X' { @bits[$i] = 'X' }
                }
            }
            my $masked = @bits.join;

            my $float = $masked.indices('X').elems;
            for ^(1 +< $float) -> $n {
                my @b = $masked.comb;
                my $b = 0;
                for ^@b.elems -> $i {
                    @b[$i] = ($n +& (1 +< $b++)) ?? '1' !! '0' if @b[$i] eq 'X';
                }
                %mem{:36(@b.join)} = $val;
            }
        }
    }
    say [+] %mem.values;
}
