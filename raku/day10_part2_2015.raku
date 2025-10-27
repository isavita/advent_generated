
sub look-and-say(Str $s is copy, Int $n) {
    for ^$n {
        $s = $s.subst(/ (.) $0* /, -> $m { $m.chars ~ $m.substr(0,1) }, :g);
    }
    $s
}

sub MAIN {
    my $in = 'input.txt'.IO.slurp.trim;
    say look-and-say($in, 50).chars
}
