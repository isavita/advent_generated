
my @c = 'input.txt'.IO.lines.map: { my @p = .split('/'); @p[0] => @p[1] };
my ($max-strength, $max-length);

sub dfs(@used, $port, $strength, $length) {
    if $length > $max-length || ($length == $max-length && $strength > $max-strength) {
        ($max-strength, $max-length) = ($strength, $length);
    }
    for ^@c -> $i {
        next if @used[$i];
        my ($a,$b) = @c[$i].kv;
        if $a == $port || $b == $port {
            @used[$i] = True;
            my $next = $a == $port ?? $b !! $a;
            dfs(@used, $next, $strength + $a + $b, $length + 1);
            @used[$i] = False;
        }
    }
}

my @u = False xx @c;
dfs(@u, 0, 0, 0);
say $max-strength;
