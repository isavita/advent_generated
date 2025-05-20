
sub MAIN() {
    my @packages = 'input.txt'.IO.slurp.lines.map(*.Int);
    my $total-weight = @packages.sum;
    my $group-weight = $total-weight / 4;

    die "Total weight not divisible by 4" unless $total-weight %% 4;

    sub multiset-subtract(@a, @b) {
        my %b-counts;
        %b-counts{$_}++ for @b;
        my @result;
        for @a -> $item {
            if %b-counts{$item} > 0 {
                %b-counts{$item}--;
            } else {
                @result.push($item);
            }
        }
        return @result;
    }

    # Check if items can be partitioned into 3 groups of target weight
    sub can-partition-three(@items, $target-weight) {
        # Try finding the second group
        for 1 .. @items.elems -> $size-b {
            for @items.combinations($size-b) -> @subset-b {
                if @subset-b.sum == $target-weight {
                    my @remaining-after-b = multiset-subtract(@items, @subset-b);

                    # Try finding the third group from the remainder
                    for 1 .. @remaining-after-b.elems -> $size-c {
                        for @remaining-after-b.combinations($size-c) -> @subset-c {
                            if @subset-c.sum == $target-weight {
                                # Found subset B and C, which implies a valid 3-group partition of @items
                                return True;
                            }
                        }
                    }
                }
            }
        }
        return False; # No such B and C found
    }

    for 1 .. @packages.elems -> $first-group-size {
        my @valid-entanglements;

        for @packages.combinations($first-group-size) -> @first-group {
            if @first-group.sum == $group-weight {
                my @remaining = multiset-subtract(@packages, @first-group);

                if can-partition-three(@remaining, $group-weight) {
                    @valid-entanglements.push([*] @first-group);
                }
            }
        }

        if @valid-entanglements.elems > 0 {
            say @valid-entanglements.min;
            exit;
        }
    }
}
