
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;
my $programStr = <$fh>;
close($fh);

my @program = ();
foreach my $s (split(',', $programStr)) {
    my $i = int($s);
    push @program, $i;
}

my $input = 5;
my $output = 0;
my $i = 0;
while (1) {
    my $opcode = $program[$i] % 100;
    my $modes = int($program[$i] / 100);
    my $param1Mode = $modes % 10;
    $modes = int($modes / 10);
    my $param2Mode = $modes % 10;

    if ($opcode == 1) {
        my $p1 = getValue(\@program, $i+1, $param1Mode);
        my $p2 = getValue(\@program, $i+2, $param2Mode);
        my $p3 = $program[$i+3];
        $program[$p3] = $p1 + $p2;
        $i += 4;
    } elsif ($opcode == 2) {
        my $p1 = getValue(\@program, $i+1, $param1Mode);
        my $p2 = getValue(\@program, $i+2, $param2Mode);
        my $p3 = $program[$i+3];
        $program[$p3] = $p1 * $p2;
        $i += 4;
    } elsif ($opcode == 3) {
        $program[$program[$i+1]] = $input;
        $i += 2;
    } elsif ($opcode == 4) {
        $output = getValue(\@program, $i+1, $param1Mode);
        print "$output\n";
        $i += 2;
    } elsif ($opcode == 5) {
        my $p1 = getValue(\@program, $i+1, $param1Mode);
        my $p2 = getValue(\@program, $i+2, $param2Mode);
        if ($p1 != 0) {
            $i = $p2;
        } else {
            $i += 3;
        }
    } elsif ($opcode == 6) {
        my $p1 = getValue(\@program, $i+1, $param1Mode);
        my $p2 = getValue(\@program, $i+2, $param2Mode);
        if ($p1 == 0) {
            $i = $p2;
        } else {
            $i += 3;
        }
    } elsif ($opcode == 7) {
        my $p1 = getValue(\@program, $i+1, $param1Mode);
        my $p2 = getValue(\@program, $i+2, $param2Mode);
        my $p3 = $program[$i+3];
        if ($p1 < $p2) {
            $program[$p3] = 1;
        } else {
            $program[$p3] = 0;
        }
        $i += 4;
    } elsif ($opcode == 8) {
        my $p1 = getValue(\@program, $i+1, $param1Mode);
        my $p2 = getValue(\@program, $i+2, $param2Mode);
        my $p3 = $program[$i+3];
        if ($p1 == $p2) {
            $program[$p3] = 1;
        } else {
            $program[$p3] = 0;
        }
        $i += 4;
    } elsif ($opcode == 99) {
        last;
    } else {
        die "Invalid opcode";
    }
}

sub getValue {
    my ($program, $pos, $mode) = @_;
    if ($mode == 0) {
        return $program->[$program->[$pos]];
    } else {
        return $program->[$pos];
    }
}
