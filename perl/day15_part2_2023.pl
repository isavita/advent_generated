use strict;
use warnings;

sub hashString {
    my $str = shift;
    my $res = 0;
    for my $char (split //, $str) {
        $res += ord($char);
        $res *= 17;
        $res %= 256;
    }
    return $res;
}

sub parseStep {
    my $stepStr = shift;
    my $step = {};

    $step->{Label} = $stepStr =~ s/[-=0123456789]//gr;
    $step->{NumBox} = hashString($step->{Label});
    $step->{Operation} = substr($stepStr, length($step->{Label}), 1);

    if ($step->{Operation} eq "=") {
        $step->{Number} = substr($stepStr, length($step->{Label}) + 1);
    }

    return $step;
}

sub getBoxes {
    my $stepsStr = shift;
    my %boxes;

    foreach my $stepStr (@$stepsStr) {
        my $step = parseStep($stepStr);
        
        if (!exists $boxes{$step->{NumBox}}) {
            $boxes{$step->{NumBox}} = [];
        }
        
        my @boxContents = @{$boxes{$step->{NumBox}}};

        if ($step->{Operation} eq "-") {
            for my $i (0..$#boxContents) {
                if (exists $boxContents[$i]->{$step->{Label}}) {
                    splice @boxContents, $i, 1;
                    last;
                }
            }
        } elsif ($step->{Operation} eq "=") {
            my $found = 0;
            foreach my $content (@boxContents) {
                if (exists $content->{$step->{Label}}) {
                    $content->{$step->{Label}} = $step->{Number};
                    $found = 1;
                    last;
                }
            }
            if (!$found) {
                push @boxContents, {$step->{Label} => $step->{Number}};
            }
        }

        if (scalar @boxContents == 0) {
            delete $boxes{$step->{NumBox}};
        } else {
            $boxes{$step->{NumBox}} = \@boxContents;
        }
    }

    return \%boxes;
}

sub calculatePower {
    my $boxes = shift;
    my $res = 0;

    for my $iBox (0..255) {
        if (exists $boxes->{$iBox}) {
            foreach my $iSlot (0..$#{$boxes->{$iBox}}) {
                foreach my $value (values %{$boxes->{$iBox}[$iSlot]}) {
                    $res += ($iBox + 1) * ($iSlot + 1) * $value;
                }
            }
        }
    }

    return $res;
}

sub solve {
    my $input = shift;
    my $line = $input->[0];
    my @stepsStr = split(',', $line);

    my $boxes = getBoxes(\@stepsStr);

    return calculatePower($boxes);
}

sub readFile {
    my $fileName = shift;
    open my $fh, '<', $fileName or die "Cannot open file: $!";
    my @lines = <$fh>;
    close $fh;
    chomp @lines;
    return \@lines;
}

my $input = readFile("input.txt");
print solve($input);