
class SNAFUConverter {

    static long snafuToDecimal(String snafu) {
        long decimal = 0
        long placeValue = 1
        for (int i = snafu.length() - 1; i >= 0; i--) {
            char c = snafu.charAt(i)
            int digitValue
            switch (c) {
                case '2': digitValue = 2; break
                case '1': digitValue = 1; break
                case '0': digitValue = 0; break
                case '-': digitValue = -1; break
                case '=': digitValue = -2; break
                default: throw new IllegalArgumentException("Invalid SNAFU digit: " + c)
            }
            decimal += digitValue * placeValue
            placeValue *= 5
        }
        return decimal
    }

    static String decimalToSnafu(long decimal) {
        if (decimal == 0) {
            return "0"
        }

        StringBuilder snafu = new StringBuilder()
        while (decimal != 0) {
            long remainder = decimal % 5
            decimal /= 5

            switch ((int) remainder) {
                case 0: snafu.append('0'); break
                case 1: snafu.append('1'); break
                case 2: snafu.append('2'); break
                case 3: snafu.append('='); decimal++; break
                case 4: snafu.append('-'); decimal++; break
            }
        }
        return snafu.reverse().toString()
    }

    static void main(String[] args) {
        long sum = 0
        File inputFile = new File("input.txt")

        if (!inputFile.exists()) {
            println("Error: input.txt not found.")
            return
        }

         inputFile.eachLine { line ->
            sum += snafuToDecimal(line.trim())
        }

        String snafuSum = decimalToSnafu(sum)
        println(snafuSum)
    }
}
