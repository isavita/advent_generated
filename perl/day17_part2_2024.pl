
use strict;
use warnings;

sub compute_operand {
    my ($val, $a, $b, $c) = @_;
    return $val if $val <= 3;
    return $a if $val == 4;
    return $b if $val == 5;
    return $c if $val == 6;
    die "Invalid combo operand: $val";
}

sub simulate_computer {
    my ($program) = @_;
    my @outs;
    my ($a, $b, $c) = ($program->{a}, $program->{b}, $program->{c});
    my @input = @{$program->{program}};

    for (my $i = 1; $i <= scalar @input; $i += 2) {
        my $cmd = $input[$i - 1];
        my $operand = compute_operand($input[$i], $a, $b, $c);
        if ($cmd == 0) {
            $a >>= $operand;
        } elsif ($cmd == 1) {
            $b ^= $input[$i];
        } elsif ($cmd == 2) {
            $b = $operand % 8;
        } elsif ($cmd == 3) {
            if ($a != 0) {
                $i = $input[$i] - 1;
            }
        } elsif ($cmd == 4) {
            $b ^= $c;
        } elsif ($cmd == 5) {
            push @outs, $operand % 8;
        } elsif ($cmd == 6) {
            $b = $a >> $operand;
        } elsif ($cmd == 7) {
            $c = $a >> $operand;
        } else {
            die "Invalid opcode: $cmd";
        }
    }
    return \@outs;
}

sub check {
    my ($p) = @_;
    my @program = @{$p->{program}};
    my @valids;
    my @stack = ([0, 0]);
    my %seen;

    while (@stack) {
        my $state = pop @stack;
        my ($depth, $score) = @$state;

        next if $seen{$state->[0] . "," . $state->[1]};
        $seen{$state->[0] . "," . $state->[1]} = 1;

        if ($depth == scalar @program) {
            push @valids, $score;
        } else {
            for my $i (0 .. 7) {
                my $new_score = $i + 8 * $score;
                my $test_program = {a => $new_score, b => $p->{b}, c => $p->{c}, program => \@program};
                my $result = simulate_computer($test_program);
                if (@$result && $result->[0] == $program[$#program - $depth]) {
                    push @stack, [$depth + 1, $new_score];
                }
            }
        }
    }
    return \@valids;
}

open my $fh, '<', 'input.txt' or die "Could not open input.txt: $!";
my ($a, $b, $c, @program);

while (<$fh>) {
    chomp;
    if (/^Register A: (\d+)/) {
        $a = $1;
    } elsif (/^Register B: (\d+)/) {
        $b = $1;
    } elsif (/^Register C: (\d+)/) {
        $c = $1;
    } elsif (/^Program: ([\d,\s]+)/) {
        @program = map { int($_) } split /,/, $1;
    }
}
close $fh;

my $p = {a => $a, b => $b, c => $c, program => \@program};
my $valid_values = check($p);
my $min_val = $valid_values->[0];
for my $val (@$valid_values) {
    $min_val = $val if $val < $min_val;
}

print "$min_val\n";
