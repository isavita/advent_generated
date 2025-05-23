
sub MAIN() {
    my $input = slurp 'input.txt';
    $input .= trim;
    say solve($input);
}

sub solve(Str $input) {
    my @parsed = parseInput($input);

    my %graph;
    for @parsed -> @pair {
        %graph{@pair[0]}{@pair[1]} = True;
        %graph{@pair[1]}{@pair[0]} = True;
    }

    my %visited = "start" => 5;
    my @path = "start";

    return walk(%graph, "start", %visited, @path, False);
}

sub walk(
    %graph,
    Str $current,
    %visited,
    @path,
    Bool $double-used is copy
) returns Int {

    if $current eq "end" {
        return 1;
    }

    %visited{$current}++;

    my $paths-to-end = 0;

    for %graph{$current}.keys -> $visitable {
        if $visitable eq "start" {
            next;
        }

        my $is-small-cave = ($visitable.lc eq $visitable);
        my $visited-count = %visited{$visitable} // 0;

        if $is-small-cave and $visited-count > 0 {
            if $double-used {
                next;
            } else {
                $double-used = True;
            }
        }

        @path.push($visitable);
        $paths-to-end += walk(%graph, $visitable, %visited, @path, $double-used);
        @path.pop;

        %visited{$visitable}--;

        if $is-small-cave and %visited{$visitable} == 1 {
            $double-used = False;
        }
    }
    return $paths-to-end;
}

sub parseInput(Str $input) {
    return $input.split("\n").map: *.split("-");
}
