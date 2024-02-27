{
    # Reset first_digit and last_digit for each line
    first_digit = ""; last_digit = "";
    
    # Find the first digit in the line
    if (match($0, /[0-9]/)) {
        first_digit = substr($0, RSTART, RLENGTH);
    }
    
    # Find the last digit in the line
    if (match($0, /[0-9][^0-9]*$/)) {
        last_digit = substr($0, RSTART, 1);
    }
    
    # If both first_digit and last_digit are found
    if (first_digit != "" && last_digit != "") {
        sum += (first_digit * 10) + last_digit;
    }
}

END {
    print sum;
}
