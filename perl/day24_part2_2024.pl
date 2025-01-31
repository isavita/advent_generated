
use strict;
use warnings;
use List::Util qw(min max);

sub parse {
    my $input = shift;
    my @parts = split /\n\n/, $input;
    return unless @parts == 2;

    my @gates;
    for (split /\n/, $parts[1]) {
        next unless $_;
        my @gate_parts = split / -> /;
        next unless @gate_parts == 2;
        my @op_parts = split / /, $gate_parts[0];
        next unless @op_parts == 3;
        push @gates, { gate => { a => $op_parts[0], op => $op_parts[1], b => $op_parts[2] }, output => $gate_parts[1] };
    }
    return @gates;
}

sub create_lookups {
    my @gates = @_;
    my %lookup;
    my %reverse_lookup;

    for my $g (@gates) {
        $lookup{$g->{output}} = $g->{gate};
        my @inputs = ($g->{gate}->{a}, $g->{gate}->{b});
        @inputs = sort @inputs;
        my $key = join '_', @inputs, $g->{gate}->{op};
        $reverse_lookup{$key} = $g->{output};
    }
    return \%lookup, \%reverse_lookup;
}

sub swap {
    my ($pairs, $gates, $a, $b) = @_;
    push @$pairs, [$a, $b];
    for my $g (@$gates) {
        if ($g->{output} eq $a) {
            $g->{output} = $b;
        } elsif ($g->{output} eq $b) {
            $g->{output} = $a;
        }
    }
}

sub get_reverse_lookup_key {
    my ($a, $op, $b) = @_;
    my @inputs = sort ($a, $b);
    return join '_', @inputs, $op;
}

sub solution {
    my @gates = @_;
    my @pairs;
    my $num_z = 0;
    for my $g (@gates) {
        $num_z++ if $g->{output} =~ /^z/;
    }

    while (@pairs < 4) {
        my $adder = '';
        my $carry = '';
        my ($lookup, $reverse_lookup) = create_lookups(@gates);

        for my $i (0 .. $num_z - 1) {
            my $xi = sprintf "x%02d", $i;
            my $yi = sprintf "y%02d", $i;
            my $zi = sprintf "z%02d", $i;
            my $bit;

            if ($i == 0) {
                $adder = $reverse_lookup->{get_reverse_lookup_key($xi, 'XOR', $yi)};
                $carry = $reverse_lookup->{get_reverse_lookup_key($xi, 'AND', $yi)};
            } else {
                $bit = $reverse_lookup->{get_reverse_lookup_key($xi, 'XOR', $yi)};
                if (defined $bit) {
                    $adder = $reverse_lookup->{get_reverse_lookup_key($bit, 'XOR', $carry)};
                    if (defined $adder) {
                        my $c1 = $reverse_lookup->{get_reverse_lookup_key($xi, 'AND', $yi)};
                        my $c2 = $reverse_lookup->{get_reverse_lookup_key($bit, 'AND', $carry)};
                        $carry = $reverse_lookup->{get_reverse_lookup_key($c1, 'OR', $c2)};
                    }
                }
            }

            if (!defined $adder) {
                my $gate = $lookup->{$zi};
                my $bit_key = get_reverse_lookup_key($xi, 'XOR', $yi);
                $bit = $reverse_lookup->{$bit_key};
                if (defined $reverse_lookup->{get_reverse_lookup_key($gate->{a}, 'XOR', $carry)}) {
                    swap(\@pairs, \@gates, $bit, $gate->{a});
                    last;
                } elsif (defined $reverse_lookup->{get_reverse_lookup_key($gate->{b}, 'XOR', $carry)}) {
                    swap(\@pairs, \@gates, $bit, $gate->{b});
                    last;
                }
            } elsif ($adder ne $zi) {
                swap(\@pairs, \@gates, $adder, $zi);
                last;
            }
        }
    }

    my @result;
    for my $pair (@pairs) {
        push @result, @$pair;
    }
    @result = sort @result;
    return join ',', @result;
}

open my $fh, '<', 'input.txt' or die "Error opening input file: $!";
my $input = do { local $/; <$fh> };
close $fh;

my @gates = parse($input);
die "Error parsing input" unless @gates;

print solution(@gates), "\n";

