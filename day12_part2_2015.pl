
use JSON;

open(my $fh, '<', 'input.txt') or die "Error opening input.txt: $!";
my $data = do { local $/; <$fh> };
close($fh);

my $jsonData = decode_json($data);

my $sum = sumNumbers($jsonData);
print $sum . "\n";

sub sumNumbers {
    my ($data) = @_;
    my $sum = 0;

    if (ref($data) eq 'ARRAY') {
        foreach my $value (@$data) {
            $sum += sumNumbers($value);
        }
    }
    elsif (ref($data) eq 'HASH') {
        unless (containsRed($data)) {
            foreach my $value (values %$data) {
                $sum += sumNumbers($value);
            }
        }
    }
    elsif (ref($data) eq '') {
        $sum += int($data);
    }

    return $sum;
}

sub containsRed {
    my ($obj) = @_;
    foreach my $value (values %$obj) {
        if (defined $value && $value eq 'red') {
            return 1;
        }
    }
    return 0;
}
