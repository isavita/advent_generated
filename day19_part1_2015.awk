
{
    if ($0 != "") {
        if (index($0, " => ") > 0) {
            replacements[length(replacements) + 1] = $0
        } else {
            molecule = $0
        }
    }
}

END {
    for (i in replacements) {
        split(replacements[i], parts, " => ")
        for (i = 1; i <= length(molecule); i++) {
            if (substr(molecule, i, length(parts[1])) == parts[1]) {
                newMolecule = substr(molecule, 1, i - 1) parts[2] substr(molecule, i + length(parts[1]))
                molecules[newMolecule] = 1
            }
        }
    }
    
    print length(molecules)
}
