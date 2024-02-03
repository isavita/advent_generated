
use Digest::MD5 qw(md5_hex);

open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";
my $doorID = <$fh>;
close($fh);

chomp($doorID);
my $password = findPassword($doorID);
print "$password\n";

sub findPassword {
    my $doorID = shift;
    my @password = ('') x 8;
    my $filledPositions = 0;
    my @found = (0) x 8;

    my $i = 0;
    while ($filledPositions < 8) {
        my $hash = md5_hex($doorID . $i);
        if ($hash =~ /^00000/) {
            my $pos = substr($hash, 5, 1);
            if ($pos =~ /^[0-7]$/) {
                my $posIndex = int($pos);
                if (!$found[$posIndex]) {
                    $found[$posIndex] = 1;
                    $password[$posIndex] = substr($hash, 6, 1);
                    $filledPositions++;
                }
            }
        }
        $i++;
    }
    return join('', @password);
}
