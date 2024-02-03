
use strict;
use warnings;

open my $fh, '<', 'input.txt' or die $!;
my $program = <$fh>;
close $fh;

my @memory = map { int } split ',', $program;
my $ip = 0;
my $relativeBase = 0;
my $output = 0;

while (1) {
    my $opcode = $memory[$ip] % 100;
    my $modes = int($memory[$ip] / 100);

    my $getParam = sub {
        my ($offset) = @_;
        my $mode = 0;
        $mode = int(substr($modes, -1 * $offset, 1)) if length($modes) >= $offset;

        my $param = $memory[$ip + $offset];
        return $mode == 0 ? $memory[$param] : $mode == 1 ? $param : $memory[$relativeBase + $param];
    };

    my $setParam = sub {
        my ($offset, $value) = @_;
        my $mode = 0;
        $mode = int(substr($modes, -1 * $offset, 1)) if length($modes) >= $offset;

        my $param = $memory[$ip + $offset];
        $memory[$param] = $value if $mode == 0;
        $memory[$relativeBase + $param] = $value if $mode == 2;
    };

    if ($opcode == 1) {
        $setParam->(3, $getParam->(1) + $getParam->(2));
        $ip += 4;
    } elsif ($opcode == 2) {
        $setParam->(3, $getParam->(1) * $getParam->(2));
        $ip += 4;
    } elsif ($opcode == 3) {
        $setParam->(1, 1); # Test mode input
        $ip += 2;
    } elsif ($opcode == 4) {
        $output = $getParam->(1);
        $ip += 2;
    } elsif ($opcode == 5) {
        $ip = $getParam->(1) != 0 ? $getParam->(2) : $ip + 3;
    } elsif ($opcode == 6) {
        $ip = $getParam->(1) == 0 ? $getParam->(2) : $ip + 3;
    } elsif ($opcode == 7) {
        $setParam->(3, $getParam->(1) < $getParam->(2) ? 1 : 0);
        $ip += 4;
    } elsif ($opcode == 8) {
        $setParam->(3, $getParam->(1) == $getParam->(2) ? 1 : 0);
        $ip += 4;
    } elsif ($opcode == 9) {
        $relativeBase += $getParam->(1);
        $ip += 2;
    } elsif ($opcode == 99) {
        print "$output\n";
        last;
    } else {
        die "unknown opcode: $opcode\n";
    }
}
