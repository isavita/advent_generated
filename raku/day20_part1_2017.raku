
sub manhattan(@a) { [+] @a».abs }

sub MAIN {
    my @particles = 'input.txt'.IO.lines.map: -> $line {
        my @parts = $line.split(', ');
        my %p;
        for @parts.kv -> $i, $part {
            my @coords = $part.substr(3,*-1).split(',').map: *.Int;
            %p<p v a>[$i] = @coords;
        }
        %p
    }

    my ($closest, $min-a, $min-v, $min-p) = 0, Inf, Inf, Inf;

    for @particles.kv -> $i, %p {
        my $a = manhattan(%p<a>);
        my $v = manhattan(%p<v>);
        my $p = manhattan(%p<p>);

        if $a < $min-a || ($a == $min-a && $v < $min-v) ||
           ($a == $min-a && $v == $min-v && $p < $min-p) {
            $min-a  = $a;
            $min-v  = $v;
            $min-p  = $p;
            $closest = $i;
        }
    }

    say $closest
}
