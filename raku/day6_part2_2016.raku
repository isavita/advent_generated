
sub MAIN {
    my @lines = 'input.txt'.IO.lines;
    my $message_len = @lines[0].chars;
    my $message1 = '';
    my $message2 = '';

    for ^$message_len -> $i {
        my %counts = @lines.map({ $_.substr($i, 1) }).BagHash;
        $message1 ~= %counts.sort({ -.value }).head.key;
        $message2 ~= %counts.sort({ +.value }).head.key;
    }

    say $message1;
    say $message2;
}
