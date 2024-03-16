use strict;
use warnings;

sub readInitialState {
    my $filename = shift;
    open(my $fh, '<', $filename) or die "Cannot open file $filename: $!";
    my $initialState = <$fh>;
    close($fh);
    chomp $initialState;
    return $initialState;
}

sub generateData {
    my ($initialState, $length) = @_;
    my $data = $initialState;
    while (length($data) < $length) {
        my $b = "";
        for (my $i = length($data) - 1; $i >= 0; $i--) {
            if (substr($data, $i, 1) eq '0') {
                $b .= '1';
            } else {
                $b .= '0';
            }
        }
        $data = $data . '0' . $b;
    }
    return substr($data, 0, $length);
}

sub calculateChecksum {
    my $data = shift;
    while (length($data) % 2 == 0) {
        my $b = "";
        for (my $i = 0; $i < length($data); $i += 2) {
            if (substr($data, $i, 1) eq substr($data, $i + 1, 1)) {
                $b .= '1';
            } else {
                $b .= '0';
            }
        }
        $data = $b;
    }
    return $data;
}

my $diskLength = 272; # Disk length for the problem
my $initialState = readInitialState("input.txt");
my $data = generateData($initialState, $diskLength);
my $checksum = calculateChecksum($data);
print "Checksum: $checksum\n";