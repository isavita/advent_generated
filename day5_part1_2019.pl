
use strict;
use warnings;

sub get_mode {
    my ($instruction, $position) = @_;
    return int($instruction / (10 ** ($position + 1))) % 10;
}

sub get_param {
    my ($program, $pointer, $mode) = @_;
    if ($mode == 0) {
        return $program->[$program->[$pointer]];
    }
    return $program->[$pointer];
}

sub run_program {
    my ($program, $input) = @_;
    my $output = 0;
    my $pointer = 0;
    while ($pointer < scalar(@$program)) {
        my $instruction = $program->[$pointer];
        my $opcode = $instruction % 100;

        if ($opcode == 1 || $opcode == 2) {
            my $param1 = get_param($program, $pointer + 1, get_mode($instruction, 1));
            my $param2 = get_param($program, $pointer + 2, get_mode($instruction, 2));
            my $result = 0;
            if ($opcode == 1) {
                $result = $param1 + $param2;
            } else {
                $result = $param1 * $param2;
            }
            $program->[$program->[$pointer + 3]] = $result;
            $pointer += 4;
        } elsif ($opcode == 3) {
            $program->[$program->[$pointer + 1]] = $input;
            $pointer += 2;
        } elsif ($opcode == 4) {
            $output = get_param($program, $pointer + 1, get_mode($instruction, 1));
            $pointer += 2;
        } elsif ($opcode == 99) {
            return $output;
        } else {
            die "Unknown opcode: $opcode";
        }
    }
    return $output;
}

open my $fh, '<', 'input.txt' or die $!;
my $data = <$fh>;
close $fh;

chomp $data;
my @str_program = split(',', $data);
my @program = map { int($_) } @str_program;

print run_program(\@program, 1) . "\n";
