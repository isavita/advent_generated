
sub is-safe-report(@levels) {
    return False if @levels.elems < 2;
    my $first-diff = @levels[1] - @levels[0];
    return False if $first-diff == 0;
    my Bool $increasing = $first-diff > 0;
    for 0 .. @levels.end - 1 -> $i {
        my $diff = @levels[$i+1] - @levels[$i];
        return False if $diff == 0;
        return False if $increasing && $diff <= 0 or !$increasing && $diff >= 0;
        my $abs-diff = $diff.abs;
        return False unless 1 <= $abs-diff <= 3;
    }
    True
}

sub is-safe-with-one-removal(@levels) {
    return False if @levels.elems <= 2;
    for 0 .. @levels.end -> $skip {
        my @mod = @levels[(0..@levels.end).grep(* != $skip)];
        return True if is-safe-report(@mod);
    }
    False
}

sub MAIN() {
    my $safe = 0;
    for 'input.txt'.IO.lines {
        my @levels = .words».Int;
        $safe++ if is-safe-report(@levels) or is-safe-with-one-removal(@levels);
    }
    say $safe;
}
