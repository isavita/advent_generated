use strict;
use warnings;

my $disk_length = 35651584;  # Disk length for the problem

sub read_initial_state {
    open my $fh, '<', 'input.txt' or die "Cannot open input.txt: $!";
    my $initial_state = <$fh>;
    chomp $initial_state;
    close $fh;
    return $initial_state;
}

sub generate_data {
    my ($initial_state, $length) = @_;
    my $data = $initial_state;
    while (length($data) < $length) {
        my $b = reverse $data;
        $b =~ tr/01/10/;
        $data = $data . '0' . $b;
    }
    return substr($data, 0, $length);
}

sub calculate_checksum {
    my $data = shift;
    while (length($data) % 2 == 0) {
        my $new_data = '';
        for (my $i = 0; $i < length($data); $i += 2) {
            $new_data .= substr($data, $i, 1) eq substr($data, $i + 1, 1) ? '1' : '0';
        }
        $data = $new_data;
    }
    return $data;
}

my $initial_state = read_initial_state();
my $data = generate_data($initial_state, $disk_length);
my $checksum = calculate_checksum($data);
print "Checksum: $checksum\n";