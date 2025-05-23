
sub MAIN {
    my $input = 'input.txt'.IO.slurp.trim;
    my @startingNumbers = $input.split(',').map(*.Int);

    my %lastSpoken; # number => turn it was last spoken
    my ($lastNumber, $nextNumber);

    for 1 .. 2020 -> $turn {
        if $turn - 1 < @startingNumbers.elems {
            $lastNumber = @startingNumbers[$turn - 1];
            %lastSpoken{$lastNumber} = $turn;
            next;
        }

        my $lastTurn;
        if %lastSpoken{$lastNumber}:exists {
            $lastTurn = %lastSpoken{$lastNumber};
            if $lastTurn != $turn - 1 {
                $nextNumber = ($turn - 1) - $lastTurn;
            } else {
                $nextNumber = 0;
            }
        } else {
            $nextNumber = 0;
        }

        %lastSpoken{$lastNumber} = $turn - 1;
        $lastNumber = $nextNumber;
    }

    $lastNumber.say;
}
