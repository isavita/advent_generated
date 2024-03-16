use strict;
use warnings;
use feature 'switch';

sub hex_to_bin {
    my $hex = shift;
    my $bin = "";
    foreach my $h (split("", $hex)) {
        my $b = hex($h);
        $bin .= sprintf("%04b", $b);
    }
    return $bin;
}

sub parse_packet {
    my ($bin_str, $idx) = @_;
    my $version = int(substr($bin_str, $idx, 1) << 2 | substr($bin_str, $idx + 1, 1) << 1 | substr($bin_str, $idx + 2, 1));
    my $type_id = int(substr($bin_str, $idx + 3, 1) << 2 | substr($bin_str, $idx + 4, 1) << 1 | substr($bin_str, $idx + 5, 1));
    $idx += 6;

    if ($type_id == 4) {
        my $value = 0;
        while (substr($bin_str, $idx, 1) == '1') {
            $value = $value << 4 | substr($bin_str, $idx + 1, 1) << 3 | substr($bin_str, $idx + 2, 1) << 2 | substr($bin_str, $idx + 3, 1) << 1 | substr($bin_str, $idx + 4, 1);
            $idx += 5;
        }
        $value = $value << 4 | substr($bin_str, $idx + 1, 1) << 3 | substr($bin_str, $idx + 2, 1) << 2 | substr($bin_str, $idx + 3, 1) << 1 | substr($bin_str, $idx + 4, 1);
        $idx += 5;
        return ($version, $idx, $value);
    }

    my $length_type_id = int(substr($bin_str, $idx, 1));
    $idx++;

    my ($num_sub_packets, $sub_packet_length);

    if ($length_type_id == 0) {
        $sub_packet_length = 0;
        for (my $i = 0; $i < 15; $i++) {
            $sub_packet_length = $sub_packet_length << 1 | substr($bin_str, $idx, 1);
            $idx++;
        }
    } else {
        $num_sub_packets = 0;
        for (my $i = 0; $i < 11; $i++) {
            $num_sub_packets = $num_sub_packets << 1 | substr($bin_str, $idx, 1);
            $idx++;
        }
    }

    my @values = ();
    while (1) {
        if ($length_type_id == 0 && $sub_packet_length == 0) {
            last;
        }
        if ($length_type_id == 1 && $num_sub_packets == 0) {
            last;
        }
        my ($new_version, $new_index, $sub_value) = parse_packet($bin_str, $idx);
        push @values, $sub_value;

        if ($length_type_id == 0) {
            $sub_packet_length -= $new_index - $idx;
        } else {
            $num_sub_packets--;
        }
        $idx = $new_index;
    }

    my $result;
    given ($type_id) {
        when (0) {
            $result = 0;
            foreach my $value (@values) {
                $result += $value;
            }
        }
        when (1) {
            $result = 1;
            foreach my $value (@values) {
                $result *= $value;
            }
        }
        when (2) {
            $result = $values[0];
            foreach my $value (@values) {
                $result = $value if $value < $result;
            }
        }
        when (3) {
            $result = $values[0];
            foreach my $value (@values) {
                $result = $value if $value > $result;
            }
        }
        when (5) {
            $result = 0;
            $result = 1 if $values[0] > $values[1];
        }
        when (6) {
            $result = 0;
            $result = 1 if $values[0] < $values[1];
        }
        when (7) {
            $result = 0;
            $result = 1 if $values[0] == $values[1];
        }
        default {
            die "Unknown typeID";
        }
    }

    return ($version, $idx, $result);
}

open my $fh, '<', 'input.txt' or die "Cannot open file: $!";
my $hex_str = <$fh>;
close $fh;

$hex_str =~ s/^\s+|\s+$//g;
my $bin_str = hex_to_bin($hex_str);
my (undef, undef, $value) = parse_packet($bin_str, 0);
print "$value\n";