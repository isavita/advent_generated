
open my $fh, '<', 'input.txt';
my $line = <$fh>;
close $fh;
chomp $line;

my @disk;
my $fileID = 0;
my $isFile = 1;
foreach my $char (split //, $line) {
    my $length = int($char);
    if ($isFile) {
        push @disk, $fileID for 1 .. $length;
        $fileID++;
    } else {
        push @disk, '.' for 1 .. $length;
    }
    $isFile = !$isFile;
}

while (1) {
    my $lfree = -1;
    for my $i (0 .. $#disk) {
        if ($disk[$i] eq '.') {
            $lfree = $i;
            last;
        }
    }
    last if $lfree == -1;

    my $rfile = -1;
    for (my $i = $#disk; $i > $lfree; $i--) {
        if ($disk[$i] ne '.') {
            $rfile = $i;
            last;
        }
    }
    last if $rfile == -1;

    $disk[$lfree] = $disk[$rfile];
    $disk[$rfile] = '.';
}

my $checksum = 0;
for my $i (0 .. $#disk) {
    $checksum += $i * $disk[$i] if $disk[$i] ne '.';
}
print $checksum;
