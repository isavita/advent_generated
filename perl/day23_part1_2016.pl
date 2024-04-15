use strict;
use warnings;

sub read_instructions {
    my ($filename) = @_;
    open my $fh, '<', $filename or die "Cannot open $filename: $!";
    my @instructions = <$fh>;
    chomp @instructions;
    close $fh;
    return @instructions;
}

sub execute_instructions {
    my ($instructions, $registers) = @_;
    my $pc = 0;
    while ($pc < @$instructions) {
        my @fields = split /\s+/, $instructions->[$pc];
        my $cmd = $fields[0];
        if ($cmd eq 'cpy') {
            my $x = get_value($fields[1], $registers);
            $registers->{$fields[2]} = $x if exists $registers->{$fields[2]};
        } elsif ($cmd eq 'inc') {
            $registers->{$fields[1]}++ if exists $registers->{$fields[1]};
        } elsif ($cmd eq 'dec') {
            $registers->{$fields[1]}-- if exists $registers->{$fields[1]};
        } elsif ($cmd eq 'jnz') {
            my $x = get_value($fields[1], $registers);
            if ($x != 0) {
                $pc += get_value($fields[2], $registers) - 1;
            }
        } elsif ($cmd eq 'tgl') {
            my $x = get_value($fields[1], $registers);
            my $tgt = $pc + $x;
            if ($tgt >= 0 && $tgt < @$instructions) {
                $instructions->[$tgt] = toggle_instruction($instructions->[$tgt]);
            }
        }
        $pc++;
    }
}

sub get_value {
    my ($s, $registers) = @_;
    return exists $registers->{$s} ? $registers->{$s} : $s;
}

sub toggle_instruction {
    my ($instr) = @_;
    my @parts = split /\s+/, $instr;
    if ($parts[0] eq 'inc') {
        $parts[0] = 'dec';
    } elsif ($parts[0] eq 'dec' || $parts[0] eq 'tgl') {
        $parts[0] = 'inc';
    } elsif ($parts[0] eq 'jnz') {
        $parts[0] = 'cpy';
    } elsif ($parts[0] eq 'cpy') {
        $parts[0] = 'jnz';
    }
    return join ' ', @parts;
}

my @instructions = read_instructions("input.txt");
my %registers = ('a' => 7, 'b' => 0, 'c' => 0, 'd' => 0);
execute_instructions(\@instructions, \%registers);
print $registers{'a'}, "\n";