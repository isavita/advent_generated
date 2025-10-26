
sub MAIN() {
    my %orbit;
    for 'input.txt'.IO.lines {
        my ($c,$s)=.split(')');
        %orbit{$s}=$c
    }
    my $tot=0;
    for %orbit.keys -> $s {
        my $c=$s;
        while %orbit{$c}:exists {
            $tot++;
            $c=%orbit{$c}
        }
    }
    say $tot
}
