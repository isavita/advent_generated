program solution;

var
    inputFile: text;
    genAStart, genBStart, genAFactor, genBFactor, modulus, genA, genB, matches, i: int64;

begin
    assign(inputFile, 'input.txt');
    reset(inputFile);

    readln(inputFile, genAStart);
    readln(inputFile, genBStart);

    genAFactor := 16807;
    genBFactor := 48271;
    modulus := 2147483647;

    genA := genAStart;
    genB := genBStart;
    matches := 0;

    for i := 1 to 40000000 do
    begin
        genA := (genA * genAFactor) mod modulus;
        genB := (genB * genBFactor) mod modulus;

        if (genA and $FFFF) = (genB and $FFFF) then
            matches := matches + 1;
    end;

    writeln(matches);

    close(inputFile);
end.