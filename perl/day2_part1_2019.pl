open(my $fh, '<', 'input.txt') or die $!;
my @inputData;
while (my $line = <$fh>) {
    chomp $line;
    my @vals = split(',', $line);
    foreach my $val (@vals) {
        push @inputData, $val;
    }
}
close($fh);

$inputData[1] = 12;
$inputData[2] = 2;

my $result = executeProgram(\@inputData);

print $result . "\n";

sub executeProgram {
    my ($data) = @_;
    for (my $i = 0; $i < scalar(@$data) - 3; $i += 4) {
        my $pos1 = $data->[$i+1];
        my $pos2 = $data->[$i+2];
        my $pos3 = $data->[$i+3];
        if ($data->[$i] == 1) {
            my $sum = $data->[$pos1] + $data->[$pos2];
            $data->[$pos3] = $sum;
        } elsif ($data->[$i] == 2) {
            my $product = $data->[$pos1] * $data->[$pos2];
            $data->[$pos3] = $product;
        } elsif ($data->[$i] == 99) {
            return $data->[0];
        } else {
            die "Invalid opcode";
        }
    }
    return $data->[0];
}