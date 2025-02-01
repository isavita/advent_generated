
#!/usr/bin/perl
use strict;
use warnings;

my $line = do {
    open my $fh, "<", "input.txt" or die "Could not open input.txt: $!";
    local $/;
    <$fh>;
};
chomp $line;

my @disk;
my $fileID = 0;
my $isFile = 1;
for my $i (0..length($line)-1) {
    my $length = substr($line, $i, 1);
    if ($isFile) {
        push @disk, ($fileID) x $length;
        $fileID++;
    } else {
        push @disk, ('.') x $length;
    }
    $isFile = !$isFile;
}

my @files;
my $curID = -1;
my $start;
for my $i (0..$#disk) {
    if ($disk[$i] eq '.') {
        $curID = -1;
        next;
    }
    my $id = $disk[$i];
    if ($id != $curID) {
        $curID = $id;
        $start = $i;
    }
    if ($i == $#disk || ($i+1 <= $#disk && $disk[$i+1] != $id)) {
        push @files, [$id, $start, $i];
    }
}

for (my $i = $#files; $i >= 0; $i--) {
    my ($id, $start, $end) = @{$files[$i]};
    my $fileLen = $end - $start + 1;
    my $leftmostSpan = -1;
    my $spanLen = 0;
    for (my $j = 0; $j < $start; $j++) {
        if ($disk[$j] eq '.') {
            if ($spanLen == 0) {
                $leftmostSpan = $j;
            }
            $spanLen++;
            if ($spanLen == $fileLen) {
                last;
            }
        } else {
            $spanLen = 0;
            $leftmostSpan = -1;
        }
    }
    if ($leftmostSpan != -1 && $spanLen == $fileLen) {
        @disk[$start..$end] = ('.') x $fileLen;
        @disk[$leftmostSpan..($leftmostSpan+$fileLen-1)] = ($id) x $fileLen;
    }
}

my $checksum = 0;
for my $i (0..$#disk) {
    if ($disk[$i] ne '.') {
        $checksum += $i * $disk[$i];
    }
}
print $checksum, "\n";
