
use strict;
use warnings;
use List::Util qw(sum);

my %modes = (0 => 'position', 1 => 'immediate', 2 => 'relative');
my %opcodes = (1 => 'add', 2 => 'mul', 3 => 'input', 4 => 'output', 5 => 'jt', 6 => 'jf', 7 => 'lt', 8 => 'eq', 9 => 'rbo', 99 => 'halt');

my $input_file = 'input.txt';
my @program = split /,/, do { local(@ARGV) = $input_file; <> };
my %data = map { $_ => $program[$_] } 0..$#program;

my ($ip, $relbase) = (0, 0);
my $output = [];
my $input = [];

while (1) {
    my $instruction = $data{$ip};
    my ($op, @modes) = decode($instruction);
    
    last if $op == 99;

    my @params = map { get_param($ip + $_ + 1, $modes[$_]) } 0..2;

    if ($op == 1) {
        set_param($ip + 3, $modes[2], $params[0] + $params[1]);
        $ip += 4;
    } elsif ($op == 2) {
        set_param($ip + 3, $modes[2], $params[0] * $params[1]);
        $ip += 4;
    } elsif ($op == 3) {
        set_param($ip + 1, $modes[0], shift @$input);
        $ip += 2;
    } elsif ($op == 4) {
        push @$output, $params[0];
        $ip += 2;
    } elsif ($op == 5) {
        $ip = $params[0] != 0 ? $params[1] : $ip + 3;
    } elsif ($op == 6) {
        $ip = $params[0] == 0 ? $params[1] : $ip + 3;
    } elsif ($op == 7) {
        set_param($ip + 3, $modes[2], $params[0] < $params[1] ? 1 : 0);
        $ip += 4;
    } elsif ($op == 8) {
        set_param($ip + 3, $modes[2], $params[0] == $params[1] ? 1 : 0);
        $ip += 4;
    } elsif ($op == 9) {
        $relbase += $params[0];
        $ip += 2;
    }
}

my %grid;
for (my $i = 0; $i < @$output; $i += 3) {
    my ($x, $y, $tile) = @$output[$i..$i+2];
    $grid{"$x,$y"} = $tile;
}

my $block_count = sum(map { $_ == 2 ? 1 : 0 } values %grid);
print $block_count;

sub decode {
    my $n = shift;
    my $op = $n % 100;
    $n = int($n / 100);
    my @modes;
    for (0..2) {
        push @modes, $n % 10;
        $n = int($n / 10);
    }
    return ($op, @modes);
}

sub get_param {
    my ($index, $mode) = @_;
    if ($mode == 0) {
        return $data{$data{$index}} // 0;
    } elsif ($mode == 1) {
        return $data{$index} // 0;
    } elsif ($mode == 2) {
        return $data{$relbase + $data{$index}} // 0;
    }
}

sub set_param {
    my ($index, $mode, $value) = @_;
    if ($mode == 0) {
        $data{$data{$index}} = $value;
    } elsif ($mode == 2) {
        $data{$relbase + $data{$index}} = $value;
    }
}
