
sub MAIN {
    my $password = slurp 'input.txt';
    $password = find-next-password($password);
    say $password;
}

sub find-next-password(Str $password) {
    my Str $new = $password;
    repeat { $new = increment-password($new) } until is-valid($new);
    $new
}

sub increment-password(Str $p) {
    my @c = $p.comb;
    for (1..8).reverse -> $i {
        @c[$i-1] = chr(ord(@c[$i-1]) + 1);
        last if @c[$i-1] le 'z';
        @c[$i-1] = 'a';
    }
    @c.join
}

sub is-valid(Str $p) {
    has-straight($p) and not contains-invalid($p) and has-two-pairs($p)
}

sub has-straight(Str $p) {
    for 0..$p.chars-3 -> $i {
        my \a = ord($p.substr($i,1));
        return True if ord($p.substr($i+1,1)) == a+1 and ord($p.substr($i+2,1)) == a+2
    }
    False
}

sub contains-invalid(Str $p) { so $p ~~ /<[iol]>/ }

sub has-two-pairs(Str $p) {
    ($p ~~ m:g/ (.)$0 /).elems >= 2
}
