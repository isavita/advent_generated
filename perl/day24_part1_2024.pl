
use strict;
use warnings;

open my $fh, '<', 'input.txt' or die "Error opening input.txt: $!";

my %wires;
my @gates;
my $parsing_wires = 1;

while (<$fh>) {
    chomp;
    if ($_ eq '') {
        $parsing_wires = 0;
        next;
    }
    if ($parsing_wires) {
        if (/^(\w+):\s*([01])$/) {
            $wires{$1} = $2;
        } else {
            die "Invalid wire definition: $_";
        }
    } else {
        if (/^(\w+)\s+(AND|OR|XOR)\s+(\w+)\s+->\s+(\w+)$/) {
            push @gates, { input1 => $1, operation => $2, input2 => $3, output => $4 };
        } else {
            die "Invalid gate definition: $_";
        }
    }
}

close $fh;

my @remaining_gates = @gates;

while (@remaining_gates) {
    my $progress = 0;
    my @new_remaining_gates;
    for my $gate (@remaining_gates) {
        my $val1 = $wires{$gate->{input1}};
        my $val2 = $wires{$gate->{input2}};
        if (defined $val1 && defined $val2) {
            my $output_val;
            if ($gate->{operation} eq 'AND') {
                $output_val = ($val1 == 1 && $val2 == 1) ? 1 : 0;
            } elsif ($gate->{operation} eq 'OR') {
                $output_val = ($val1 == 1 || $val2 == 1) ? 1 : 0;
            } elsif ($gate->{operation} eq 'XOR') {
                $output_val = ($val1 != $val2) ? 1 : 0;
            }
            $wires{$gate->{output}} = $output_val;
            $progress = 1;
        } else {
            push @new_remaining_gates, $gate;
        }
    }
    unless ($progress) {
        die "Cannot evaluate remaining gates due to missing inputs or cyclic dependencies.";
    }
    @remaining_gates = @new_remaining_gates;
}

my %z_wires;
for my $wire (keys %wires) {
    if ($wire =~ /^z(\d+)$/) {
        $z_wires{$1} = $wires{$wire};
    }
}

my @indices = sort { $b <=> $a } keys %z_wires;
my $binary_string = '';
$binary_string .= $z_wires{$_} for @indices;

print oct("0b$binary_string"), "\n";
