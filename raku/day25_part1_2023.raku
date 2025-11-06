
sub MAIN() {
    my @input = 'input.txt'.IO.lines;
    my %graph;

    for @input -> $line {
        my ($v,$rest) = $line.split(': ');
        for $rest.split(' ') -> $t {
            %graph{$v}{$t} = 1;
            %graph{$t}{$v} = 1;
        }
    }

    my $source = %graph.keys[0];

    my ($g1,$g2);
    for %graph.keys -> $end {
        next if $end eq $source;
        my %g = deep-copy-edges(%graph);
        for ^3 {
            my %from = bfs(%g,$source,$end);
            my @path = build-path($source,$end,%from);
            for 0..@path-2 -> $i {
                my $a=@path[$i]; my $b=@path[$i+1];
                %g{$a}{$b}:delete;
                %g{$b}{$a}:delete;
            }
        }
        my %from = bfs(%g,$source,$end);
        unless %from{$end}:exists {
            $g1 = %from.keys.elems;
            $g2 = %graph.keys.elems - $g1;
            last;
        }
    }

    say $g1 * $g2;
}

sub deep-copy-edges(%g) {
    my %h;
    for %g.kv -> $k,%v {
        for %v.kv -> $kk,$vv {
            %h{$k}{$kk} = $vv;
        }
    }
    %h
}

sub bfs(%g,$start,$goal) {
    my @q = $start;
    my %from = $start => $start;
    while @q {
        my $cur = @q.shift;
        return %from if $cur eq $goal;
        for %g{$cur}.keys -> $n {
            unless %from{$n}:exists {
                %from{$n} = $cur;
                @q.push: $n;
            }
        }
    }
    %from
}

sub build-path($s,$e,%from) {
    my @p;
    my $c = $e;
    while $c ne $s {
        @p.unshift($c);
        $c = %from{$c};
    }
    @p.unshift($s);
    @p;
}
