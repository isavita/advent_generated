#include <fstream>
#include <iostream>

using namespace std;

int transform(int subjectNumber, int loopSize) {
    int value = 1;
    for (int i = 0; i < loopSize; i++) {
        value = (long long)value * subjectNumber % 20201227;
    }
    return value;
}

int findLoopSize(int publicKey) {
    int value = 1;
    int loopSize = 0;
    while (value != publicKey) {
        value = (long long)value * 7 % 20201227;
        loopSize++;
    }
    return loopSize;
}

int main() {
    ifstream file("input.txt");
    int cardPublicKey, doorPublicKey;
    file >> cardPublicKey >> doorPublicKey;

    int cardLoopSize = findLoopSize(cardPublicKey);
    int encryptionKey = transform(doorPublicKey, cardLoopSize);

    cout << encryptionKey << endl;

    return 0;
}