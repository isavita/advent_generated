
sub MAIN {
    my %id;
    my @name;
    my @adj;

    for 'input.txt'.IO.lines {
        my ($a,$b) = .split('-');
        %id{$a} //= +@name.push($a) - 1;
        %id{$b} //= +@name.push($b) - 1;
        @adj[%id{$a}][%id{$b}] = True;
        @adj[%id{$b}][%id{$a}] = True;
    }

    my $n = @name.elems;
    my $count = 0;

    for 0..$n-1 -> $i {
        for $i+1..$n-1 -> $j {
            next unless @adj[$i][$j];
            for $j+1..$n-1 -> $k {
                if @adj[$j][$k] && @adj[$k][$i] {
                    $count++ if @name[$i].starts-with('t')
                           || @name[$j].starts-with('t')
                           || @name[$k].starts-with('t');
                }
            }
        }
    }

    say $count;
}
