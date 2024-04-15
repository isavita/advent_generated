use strict;
use warnings;

my %instructions = (
    'addr' => sub { $_[0][$_[1]] + $_[0][$_[2]] },
    'addi' => sub { $_[0][$_[1]] + $_[2] },
    'mulr' => sub { $_[0][$_[1]] * $_[0][$_[2]] },
    'muli' => sub { $_[0][$_[1]] * $_[2] },
    'banr' => sub { $_[0][$_[1]] & $_[0][$_[2]] },
    'bani' => sub { $_[0][$_[1]] & $_[2] },
    'borr' => sub { $_[0][$_[1]] | $_[0][$_[2]] },
    'bori' => sub { $_[0][$_[1]] | $_[2] },
    'setr' => sub { $_[0][$_[1]] },
    'seti' => sub { $_[1] },
    'gtir' => sub { $_[1] > $_[0][$_[2]] ? 1 : 0 },
    'gtri' => sub { $_[0][$_[1]] > $_[2] ? 1 : 0 },
    'gtrr' => sub { $_[0][$_[1]] > $_[0][$_[2]] ? 1 : 0 },
    'eqir' => sub { $_[1] == $_[0][$_[2]] ? 1 : 0 },
    'eqri' => sub { $_[0][$_[1]] == $_[2] ? 1 : 0 },
    'eqrr' => sub { $_[0][$_[1]] == $_[0][$_[2]] ? 1 : 0 },
);

sub load_program {
    my @lines = @_;
    my @program;
    my $ip_register;

    foreach my $line (@lines) {
        if ($line =~ /^#ip (\d+)/) {
            $ip_register = $1;
            next;
        }

        my ($op, $a, $b, $c) = $line =~ /(\w+) (\d+) (\d+) (\d+)/;
        push @program, sub { $_[0][$c] = $instructions{$op}->($_[0], $a, $b) };
    }

    return ($ip_register, \@program);
}

sub run_program {
    my ($ip_register, $program, $registers, $max_cycles) = @_;
    my $ip = 0;
    my $cycles = 0;

    while ($ip >= 0 && $ip < @$program) {
        $registers->[$ip_register] = $ip;
        $program->[$ip]->($registers);
        $ip = $registers->[$ip_register] + 1;
        $cycles++;
        last if $max_cycles > 0 && $cycles >= $max_cycles;
    }

    return $registers;
}

sub max {
    my $max_value = shift;
    foreach my $value (@_) {
        $max_value = $value if $value > $max_value;
    }
    return $max_value;
}

open my $fh, '<', 'input.txt' or die "Could not open file 'input.txt': $!";
my @lines = <$fh>;
close $fh;
chomp @lines;
@lines = grep { $_ ne '' } @lines;

my ($ip_register, $program) = load_program(@lines);

my @registers = (1, 0, 0, 0, 0, 0);
@registers = @{run_program($ip_register, $program, \@registers, 1000)};
my $n = max(@registers);
my $total = 0;
for my $i (1 .. $n) {
    $total += $i if $n % $i == 0;
}
print "$total\n";