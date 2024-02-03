
use Digest::MD5 qw(md5_hex);

my @queue = ({x => 0, y => 0, path => ""});
my $longest = 0;

open(my $fh, '<', 'input.txt') or die "Failed to open file: $!";
my $passcode = <$fh>;
close($fh);

while (@queue) {
    my $point = shift @queue;

    if ($point->{x} == 3 && $point->{y} == 3) {
        $longest = length($point->{path}) if length($point->{path}) > $longest;
        next;
    }

    my @doors = getOpenDoors($passcode, $point->{path});
    foreach my $dir (@doors) {
        my %nextPoint = (x => $point->{x}, y => $point->{y}, path => $point->{path} . $dir);
        $nextPoint{y}-- if $dir eq "U";
        $nextPoint{y}++ if $dir eq "D";
        $nextPoint{x}-- if $dir eq "L";
        $nextPoint{x}++ if $dir eq "R";

        if ($nextPoint{x} >= 0 && $nextPoint{x} < 4 && $nextPoint{y} >= 0 && $nextPoint{y} < 4) {
            push @queue, \%nextPoint;
        }
    }
}

print "$longest\n";

sub getOpenDoors {
    my ($passcode, $path) = @_;
    my $hash = md5_hex($passcode . $path);
    my @doors;

    push @doors, "U" if ord(substr($hash, 0, 1)) >= ord('b') && ord(substr($hash, 0, 1)) <= ord('f');
    push @doors, "D" if ord(substr($hash, 1, 1)) >= ord('b') && ord(substr($hash, 1, 1)) <= ord('f');
    push @doors, "L" if ord(substr($hash, 2, 1)) >= ord('b') && ord(substr($hash, 2, 1)) <= ord('f');
    push @doors, "R" if ord(substr($hash, 3, 1)) >= ord('b') && ord(substr($hash, 3, 1)) <= ord('f');

    return @doors;
}
