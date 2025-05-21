
sub MAIN {
    my @groups = 'input.txt'.IO.slurp.split("\n\n");

    my $part1 = @groups.map({ $_.trans("\n" => "").comb.unique.elems }).sum;
    say $part1;

    my $part2 = @groups.map(-> $group {
        my @people = $group.split("\n");
        my $num-people = @people.elems;
        my $bag = @people.join('').comb.Bag;
        $bag.values.grep({ $_ == $num-people }).elems
    }).sum;
    say $part2;
}
