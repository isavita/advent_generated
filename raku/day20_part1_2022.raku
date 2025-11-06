
sub MAIN {
    my @in = 'input.txt'.IO.lines».Int;
    my @n = @in.map: { %( pos => $++, val => $_ ) };
    my @n2 = @n.map: { %( pos => $_<pos>, val => 811589153 * $_<val> ) };

    mix(@n);
    say coords(@n);
}

sub mix(@nums) {
    my $m = @nums - 1;
    for @nums -> $x {
        my $op = $x<pos>;
        my $np = (($op + $x<val>) % $m + $m) % $m;
        if $op < $np {
            for @nums -> $y { $y<pos>-- if $y<pos> > $op and $y<pos> <= $np }
        }
        if $np < $op {
            for @nums -> $y { $y<pos>++ if $y<pos> >= $np and $y<pos> < $op }
        }
        $x<pos> = $np;
    }
}

sub coords(@nums) {
    my $z = @nums.first(*<val> == 0)<pos>;
    my $m = @nums.elems;
    sum @nums.grep( { my $p = $_<pos>;
                       $p == ($z + 1000) % $m
                    || $p == ($z + 2000) % $m
                    || $p == ($z + 3000) % $m } )»<val>
}
