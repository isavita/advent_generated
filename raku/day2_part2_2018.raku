
sub part_one ( @box-ids ) {
    my $count-two = 0;
    my $count-three = 0;
    for @box-ids -> $box-id {
        my %letter-count = $box-id.comb.Bag;
        if %letter-count.values.grep(2).elems > 0 {
            $count-two++;
        }
        if %letter-count.values.grep(3).elems > 0 {
            $count-three++;
        }
    }
    $count-two * $count-three;
}

sub part_two ( @box-ids ) {
    for @box-ids.combinations(2) -> @pair {
        my ($id1, $id2) = @pair;
        my @zipped = $id1.comb Z $id2.comb;
        my @diffs = @zipped.grep: { $_[0] ne $_[1] };
        if @diffs.elems == 1 {
            my @common = @zipped.grep: { $_[0] eq $_[1] };
            return @common.map(*[0]).join;
        }
    }
    '';
}

sub MAIN {
    my @box-ids = "input.txt".IO.lines.map: *.chomp;
    say part_one(@box-ids);
    say part_two(@box-ids);
}

