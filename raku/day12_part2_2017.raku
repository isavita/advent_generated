
sub MAIN {
    my %adj;

    for "input.txt".IO.lines -> $line {
        my ($from-str, $to-str) = $line.split(" <-> ");
        my $from = $from-str.Int;
        my @to = $to-str.split(", ").map(*.Int);

        %adj{$from}.push(@to);

        for @to -> $to {
            %adj{$to}.push($from);
        }
    }

    my %visited;
    my $groups = 0;

    sub DFS($node is copy) {
        %visited{$node} = True;
        for %adj{$node}.sort.unique -> $neighbor {
            unless %visited{$neighbor}.defined {
                DFS($neighbor);
            }
        }
    }

    for %adj.keys.sort -> $node {
        unless %visited{$node}.defined {
            DFS($node);
            $groups++;
        }
    }

    say $groups;
}
