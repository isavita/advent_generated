
sub MAIN() {
    my $content = 'input.txt'.IO.slurp;
    my @blocks = $content.split(/\n\n/);

    my @seeds = @blocks[0].split(':')[1].words.map(*.Int);

    my @maps;
    for @blocks[1..*] -> $block {
        my @ranges;
        for $block.lines[1..*] -> $line {
            my @nums = $line.words.map(*.Int);
            if @nums.elems == 3 {
                @ranges.push([@nums[1], @nums[0], @nums[2]]); # Store as [src_start, dest_start, length]
            }
        }
        @maps.push(@ranges);
    }

    my $min-location = +Inf; # Use +Inf for initial minimum comparison

    for @seeds -> $seed {
        my $location = $seed;
        for @maps -> @map {
            my $converted = $location; # Default: not converted
            for @map -> @range {
                my ($src-start, $dest-start, $length) = @range;
                if $location >= $src-start and $location < $src-start + $length {
                    $converted = $dest-start + ($location - $src-start);
                    last; # Found the mapping, move to the next map
                }
            }
            $location = $converted;
        }
        $min-location = min($min-location, $location);
    }

    print $min-location;
}

