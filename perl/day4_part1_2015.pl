
use Digest::MD5 qw(md5_hex);

open(my $fh, '<', 'input.txt') or die $!;
my $secretKey = <$fh>;
close($fh);

chomp($secretKey);
my $number = 0;

while (1) {
    my $hash = md5_hex($secretKey . $number);
    
    if (substr($hash, 0, 5) eq "00000") {
        print "$number\n";
        last;
    } else {
        $number++;
    }
}
