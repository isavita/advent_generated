#!/usr/bin/perl
use strict;
use warnings;

sub is_register {
    my ($x) = @_;
    return length($x) == 1 && $x =~ /^[abcd]$/;
}

sub get_value {
    my ($x, $regs) = @_;
    if (is_register($x)) {
        return $regs->{$x};
    } else {
        return int($x);
    }
}

sub execute_program {
    my ($instructions_in, $regs) = @_;
    my @instructions = @{$instructions_in};
    my $n = scalar @instructions;
    my $i = 0;

    while ($i < $n) {
        # Optimization: detect 6-instruction multiplication pattern
        if ($i + 5 < $n) {
            my @p1 = split /\s+/, $instructions[$i];
            my @p2 = split /\s+/, $instructions[$i+1];
            my @p3 = split /\s+/, $instructions[$i+2];
            my @p4 = split /\s+/, $instructions[$i+3];
            my @p5 = split /\s+/, $instructions[$i+4];
            my @p6 = split /\s+/, $instructions[$i+5];

            if (
                scalar(@p1) == 3 && $p1[0] eq 'cpy' &&
                scalar(@p2) == 2 && $p2[0] eq 'inc' &&
                scalar(@p3) == 2 && $p3[0] eq 'dec' &&
                scalar(@p4) == 3 && $p4[0] eq 'jnz' &&
                scalar(@p5) == 2 && $p5[0] eq 'dec' &&
                scalar(@p6) == 3 && $p6[0] eq 'jnz'
            ) {
                my $cpy_x = $p1[1];
                my $cpy_y = $p1[2];
                my $inc_a = $p2[1];
                my $dec_c = $p3[1];
                my $jnz_c = $p4[1];
                my $jnz_c_offset_str = $p4[2];
                my $dec_d = $p5[1];
                my $jnz_d = $p6[1];
                my $jnz_d_offset_str = $p6[2];

                my $jnz_c_offset = ($jnz_c_offset_str =~ /^-?\d+$/) ? int($jnz_c_offset_str) : undef;
                my $jnz_d_offset = ($jnz_d_offset_str =~ /^-?\d+$/) ? int($jnz_d_offset_str) : undef;

                if (defined $jnz_c_offset && defined $jnz_d_offset &&
                    is_register($cpy_y) && is_register($inc_a) && is_register($dec_c) &&
                    is_register($jnz_c) && is_register($dec_d) && is_register($jnz_d) &&
                    $inc_a eq 'a' && $dec_c eq $cpy_y && $jnz_c eq $cpy_y &&
                    $jnz_c_offset == -2 && $dec_d eq 'd' && $jnz_d eq 'd' && $jnz_d_offset == -5) {

                    my $val_cpy_x = get_value($cpy_x, $regs);
                    my $val_d = get_value($dec_d, $regs);
                    $regs->{a} += $val_cpy_x * $val_d;
                    $regs->{$cpy_y} = 0;
                    $regs->{d} = 0;
                    $i += 6;
                    next;
                }
            }
        }

        # Normal instruction execution
        my @parts = split /\s+/, $instructions[$i];
        my $cmd = $parts[0];

        if ($cmd eq 'tgl' && scalar(@parts) == 2) {
            my $x = $parts[1];
            my $offset = get_value($x, $regs);
            my $target_idx = $i + $offset;
            if ($target_idx >= 0 && $target_idx < $n) {
                my $target_instruction = $instructions[$target_idx];
                my @target_parts = split /\s+/, $target_instruction;
                my $target_cmd = $target_parts[0];

                if (scalar(@target_parts) == 2) {
                    if ($target_cmd eq 'inc') {
                        $target_parts[0] = 'dec';
                    } else {
                        $target_parts[0] = 'inc';
                    }
                } elsif (scalar(@target_parts) == 3) {
                    if ($target_cmd eq 'jnz') {
                        $target_parts[0] = 'cpy';
                    } else {
                        $target_parts[0] = 'jnz';
                    }
                }
                $instructions[$target_idx] = join ' ', @target_parts;
            }
            $i++;
        } elsif ($cmd eq 'cpy' && scalar(@parts) == 3) {
            my ($x, $y) = ($parts[1], $parts[2]);
            if (is_register($y)) {
                $regs->{$y} = get_value($x, $regs);
            }
            $i++;
        } elsif ($cmd eq 'inc' && scalar(@parts) == 2) {
            my $x = $parts[1];
            if (is_register($x)) {
                $regs->{$x}++;
            }
            $i++;
        } elsif ($cmd eq 'dec' && scalar(@parts) == 2) {
            my $x = $parts[1];
            if (is_register($x)) {
                $regs->{$x}--;
            }
            $i++;
        } elsif ($cmd eq 'jnz' && scalar(@parts) == 3) {
            my ($x, $y) = ($parts[1], $parts[2]);
            my $val_x = get_value($x, $regs);
            if ($val_x != 0) {
                my $jump = get_value($y, $regs);
                $i += $jump;
            } else {
                $i++;
            }
        } else {
            $i++;
        }
    }
}

sub main {
    open my $fh, '<', 'input.txt' or die "Can't open input.txt: $!";
    my @instructions = <$fh>;
    close $fh;
    chomp @instructions;

    my %registers = ( a => 12, b => 0, c => 0, d => 0 );
    execute_program(\@instructions, \%registers);
    print $registers{a}, "\n";
}

main();