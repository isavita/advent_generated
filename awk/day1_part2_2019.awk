
{
    total += calcFuelMass($1)
}
END {
    print int(total)
}

function calcFuelMass(mass) {
    fuel = int(mass/3) - 2
    if (fuel <= 0) {
        return 0
    }
    return fuel + calcFuelMass(fuel)
}
