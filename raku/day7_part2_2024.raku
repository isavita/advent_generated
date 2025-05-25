sub concat(Int $a, Int $b) {
    "$a$b".Int
}

sub canProduce(Int $target, @nums, Int $idx, Int $value) {
    if $idx == @nums.elems {
        return $value == $target;
    }

    my $n = @nums[$idx];

    if canProduce($target, @nums, $idx + 1, $value + $n) {
        return True;
    }
    if canProduce($target, @nums, $idx + 1, $value * $n) {
        return True;
    }
    if canProduce($target, @nums, $idx + 1, concat($value, $n)) {
        return True;
    }

    return False;
}

sub MAIN() {
    my $total = 0;

    for "input.txt".IO.lines -> $line {
        next if $line.trim eq '';

        my @parts = $line.split(':');
        my $target = @parts[0].trim.Int;
        my @nums = @parts[1].trim.words.map(*.Int);

        next if @nums.elems == 0;

        if @nums.elems == 1 {
            if @nums[0] == $target {
                $total += $target;
            }
            next;
        }

        if canProduce($target, @nums, 1, @nums[0]) {
            $total += $target;
        }
    }

    say $total;
}