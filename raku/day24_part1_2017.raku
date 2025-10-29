
sub strongest(@c, @u, $p, $s) {
    my $m = $s;
    for ^@c -> $i {
        next if @u[$i];
        my ($a,$b) = @c[$i];
        if $a == $p || $b == $p {
            @u[$i] = True;
            my $np = $a == $p ?? $b !! $a;
            $m = max $m, strongest(@c, @u, $np, $s + $a + $b);
            @u[$i] = False;
        }
    }
    $m;
}

sub MAIN() {
    my @comp = "input.txt".IO.lines.map({ .split('/')>>.Int });
    my @used = (False) xx @comp.elems;
    say strongest(@comp, @used, 0, 0);
}
