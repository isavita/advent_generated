
sub trimLeadingZeros(Str $s --> Str) {
    $s.subst(/^0+/, '') || '0'
}

sub splitStone(Str $s --> List) {
    my $mid = $s.chars / 2;
    my $left  = trimLeadingZeros($s.substr(0, $mid));
    my $right = trimLeadingZeros($s.substr($mid));
    ($left, $right)
}

sub MAIN() {
    my %stones;
    for 'input.txt'.IO.slurp.words -> $s {
        %stones{$s}++;
    }

    constant STEPS = 75;
    for ^STEPS {
        my %new-stones;
        for %stones.kv -> $stone, $count {
            if $stone eq "0" {
                %new-stones{"1"} += $count;
            } elsif $stone.chars % 2 == 0 {
                my ($left, $right) = splitStone($stone);
                %new-stones{$left}  += $count;
                %new-stones{$right} += $count;
            } else {
                %new-stones{($stone.Int * 2024).Str} += $count;
            }
        }
        %stones = %new-stones;
    }

    say %stones.values.sum;
}
