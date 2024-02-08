
def file = new File("input.txt")
def expenses = file.readLines().collect { it as int }

for (int i = 0; i < expenses.size(); i++) {
    for (int j = i + 1; j < expenses.size(); j++) {
        for (int k = j + 1; k < expenses.size(); k++) {
            if (expenses[i] + expenses[j] + expenses[k] == 2020) {
                println(expenses[i] * expenses[j] * expenses[k])
                return
            }
        }
    }
}
