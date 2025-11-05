
use v6.d;

sub dfs($program, @adj, @visited) {
    @visited[$program] = True;
    for @adj[$program].keys -> $i {
        dfs($i, @adj, @visited) unless @visited[$i];
    }
}

sub MAIN {
    my @adj;
    for 'input.txt'.IO.lines {
        my ($program, $rest) = .split: ' <-> ';
        my $p = +$program;
        @adj[$p] //= SetHash.new;
        for $rest.split(', ') -> $t {
            my $q = +$t;
            @adj[$p]{$q} = True;
            @adj[$q] //= SetHash.new;
            @adj[$q]{$p} = True;
        }
    }

    my @visited;
    dfs(0, @adj, @visited);

    say +@visited.grep: *.so;
}
