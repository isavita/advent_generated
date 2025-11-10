
sub MAIN {
    my $fh = open "input.txt", :r or die "Cannot open file: $!";
    my $first-point = [0, 0];
    my $last-point = $first-point;
    my $shoelace-sum = 0;
    my $perimeter = 0;

    my %directions = (
        'U' => [0, -1],
        'L' => [-1, 0],
        'D' => [0, 1],
        'R' => [1, 0]
    );

    for $fh.lines -> $line {
        my ($dir, $len) = $line.split(/\s+/);
        my $length = $len.Int;
        my $vec = %directions{$dir} // die "Invalid direction: $dir";
        my $new-x = $last-point[0] + $vec[0] * $length;
        my $new-y = $last-point[1] + $vec[1] * $length;
        my $new-point = [ $new-x, $new-y ];

        $shoelace-sum += $last-point[0] * $new-point[1] - $new-point[0] * $last-point[1];
        $perimeter += ($last-point[0] - $new-point[0]).abs() + ($last-point[1] - $new-point[1]).abs();

        $last-point = $new-point;
    }
    $fh.close;

    $shoelace-sum += $last-point[0] * $first-point[1] - $first-point[0] * $last-point[1];
    $perimeter += ($last-point[0] - $first-point[0]).abs() + ($last-point[1] - $first-point[1]).abs();

    my $shoelace-area = $shoelace-sum.abs() div 2;
    my $area = $shoelace-area + $perimeter div 2 + 1;
    say $area;
}
