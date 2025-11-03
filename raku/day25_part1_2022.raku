
sub from-snafu(Str $s) {
    my Int $n = 0;
    for $s.comb -> $d {
        $n *= 5;
        $n += $d eq '=' ?? -2
             !! $d eq '-' ?? -1
             !! $d - '0';
    }
    $n
}

sub to-snafu(Int $n is copy) {
    my Str $out = '';
    while $n > 0 {
        given $n % 5 {
            when 3 { $n += 5; $out ~= '=' }
            when 4 { $n += 5; $out ~= '-' }
            default { $out ~= Str($_) }
        }
        $n = ($n / 5).floor
    }
    $out.flip
}

sub MAIN() {
    my $sum = [+] 'input.txt'.IO.lines.map: { from-snafu($_) };
    say to-snafu($sum)
}
