
use JSON;

open(my $fh, "<", "input.txt") or die "Error reading input: $!";
my $data;
{
    local $/;
    $data = <$fh>;
}
close($fh);

my $jsonData = decode_json($data);

my $sum = sumNumbers($jsonData);
print $sum . "\n";

sub sumNumbers {
    my $data = shift;
    my $sum = 0;
    if (ref($data) eq 'ARRAY') {
        foreach my $v (@$data) {
            $sum += sumNumbers($v);
        }
    }
    elsif (ref($data) eq 'HASH') {
        foreach my $v (values %$data) {
            $sum += sumNumbers($v);
        }
    }
    elsif (ref($data) eq '') {
        $sum += int($data);
    }
    return $sum;
}
