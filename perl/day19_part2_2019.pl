use strict;
use warnings;
use 5.010;

sub read_code {
    open my $fh, '<', 'input.txt' or die "Cannot open file: $!";
    my $line = <$fh>;
    chomp $line;
    my @code = split /,/, $line;
    return \@code;
}

sub get_param_address {
    my ($code, $ip, $mode, $relative_base) = @_;
    if ($mode == 0) {
        return $code->[$ip];
    } elsif ($mode == 1) {
        return $ip;
    } elsif ($mode == 2) {
        return $relative_base + $code->[$ip];
    }
    die "Invalid mode";
}

sub run_vm {
    my ($code, $input) = @_;
    my $ip = 0;
    my $relative_base = 0;
    my $output;

    while (1) {
        my $cmd = $code->[$ip];
        my $opcode = $cmd % 100;
        my $mode1 = int($cmd / 100) % 10;
        my $mode2 = int($cmd / 1000) % 10;
        my $mode3 = int($cmd / 10000) % 10;

        if ($opcode == 1) {
            my $addr1 = get_param_address($code, $ip + 1, $mode1, $relative_base);
            my $addr2 = get_param_address($code, $ip + 2, $mode2, $relative_base);
            my $addr3 = get_param_address($code, $ip + 3, $mode3, $relative_base);
            $code->[$addr3] = $code->[$addr1] + $code->[$addr2];
            $ip += 4;
        } elsif ($opcode == 2) {
            my $addr1 = get_param_address($code, $ip + 1, $mode1, $relative_base);
            my $addr2 = get_param_address($code, $ip + 2, $mode2, $relative_base);
            my $addr3 = get_param_address($code, $ip + 3, $mode3, $relative_base);
            $code->[$addr3] = $code->[$addr1] * $code->[$addr2];
            $ip += 4;
        } elsif ($opcode == 3) {
            my $addr1 = get_param_address($code, $ip + 1, $mode1, $relative_base);
            $code->[$addr1] = shift @$input;
            $ip += 2;
        } elsif ($opcode == 4) {
            my $addr1 = get_param_address($code, $ip + 1, $mode1, $relative_base);
            $output = $code->[$addr1];
            $ip += 2;
            last;
        } elsif ($opcode == 5) {
            my $addr1 = get_param_address($code, $ip + 1, $mode1, $relative_base);
            my $addr2 = get_param_address($code, $ip + 2, $mode2, $relative_base);
            if ($code->[$addr1] != 0) {
                $ip = $code->[$addr2];
            } else {
                $ip += 3;
            }
        } elsif ($opcode == 6) {
            my $addr1 = get_param_address($code, $ip + 1, $mode1, $relative_base);
            my $addr2 = get_param_address($code, $ip + 2, $mode2, $relative_base);
            if ($code->[$addr1] == 0) {
                $ip = $code->[$addr2];
            } else {
                $ip += 3;
            }
        } elsif ($opcode == 7) {
            my $addr1 = get_param_address($code, $ip + 1, $mode1, $relative_base);
            my $addr2 = get_param_address($code, $ip + 2, $mode2, $relative_base);
            my $addr3 = get_param_address($code, $ip + 3, $mode3, $relative_base);
            $code->[$addr3] = $code->[$addr1] < $code->[$addr2] ? 1 : 0;
            $ip += 4;
        } elsif ($opcode == 8) {
            my $addr1 = get_param_address($code, $ip + 1, $mode1, $relative_base);
            my $addr2 = get_param_address($code, $ip + 2, $mode2, $relative_base);
            my $addr3 = get_param_address($code, $ip + 3, $mode3, $relative_base);
            $code->[$addr3] = $code->[$addr1] == $code->[$addr2] ? 1 : 0;
            $ip += 4;
        } elsif ($opcode == 9) {
            my $addr1 = get_param_address($code, $ip + 1, $mode1, $relative_base);
            $relative_base += $code->[$addr1];
            $ip += 2;
        } elsif ($opcode == 99) {
            last;
        } else {
            die "Unknown opcode $opcode";
        }
    }
    return $output;
}

sub beam {
    my ($x, $y) = @_;
    my $code = read_code();
    my $output = run_vm($code, [$x, $y]);
    return $output == 1;
}

my $y = 20;
my $x = 0;

while (1) {
    if (!beam($x, $y)) {
        $x++;
        next;
    }
    if (!beam($x + 99, $y)) {
        $y++;
        next;
    }
    if (!beam($x, $y + 99)) {
        $x++;
        next;
    }
    say $x * 10000 + $y;
    last;
}