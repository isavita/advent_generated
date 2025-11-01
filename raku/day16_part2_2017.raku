
sub spin(@p, $x) { @p.rotate(-$x) }
sub exchange(@p, $a, $b) { @p[$a, $b] = @p[$b, $a]; @p }
sub partner(@p, $a, $b) {
    my $i = @p.first(* eq $a, :k);
    my $j = @p.first(* eq $b, :k);
    exchange(@p, $i, $j)
}
sub dance(@p, @m) {
    for @m {
        when /^s(\d+)/     { @p = spin(@p, +$0) }
        when /^x(\d+)\/(\d+)/ { @p = exchange(@p, +$0, +$1) }
        when /^p(\w)\/(\w)/  { @p = partner(@p, $0, $1) }
    }
    @p
}
my @m = slurp('input.txt').trim.split(',');
my @p = 'a'..'p';
my %seen;
my $rep = 1_000_000_000;
my ($start, $len);
loop (my $i = 0; $i < $rep; $i++) {
    my $key = @p.join;
    if %seen{$key}:exists {
        $start = %seen{$key};
        $len = $i - $start;
        last;
    }
    %seen{$key} = $i;
    @p = dance(@p, @m);
}
if $len {
    $rep = ($rep - $start) % $len + $start;
    @p = %seen.pairs.first({.value == $rep}).key.comb;
}
put @p.join;
