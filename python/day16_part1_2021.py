
import sys

def hexToBin(hex):
    bin = ""
    for h in hex:
        b = int(h, 16)
        bin += format(b, '04b')
    return bin

def parsePacket(binStr, idx):
    version = int(binStr[idx]) << 2 | int(binStr[idx + 1]) << 1 | int(binStr[idx + 2])
    typeID = int(binStr[idx + 3]) << 2 | int(binStr[idx + 4]) << 1 | int(binStr[idx + 5])
    idx += 6

    if typeID == 4:
        while binStr[idx] == '1':
            idx += 5
        idx += 5
        return version, idx

    lengthTypeID = int(binStr[idx])
    idx += 1
    numSubPackets = 0
    subPacketLength = 0

    if lengthTypeID == 0:
        subPacketLength = 0
        for i in range(15):
            subPacketLength = subPacketLength << 1 | int(binStr[idx])
            idx += 1
    else:
        numSubPackets = 0
        for i in range(11):
            numSubPackets = numSubPackets << 1 | int(binStr[idx])
            idx += 1

    versionSum = version
    while True:
        if lengthTypeID == 0 and subPacketLength == 0:
            break
        if lengthTypeID == 1 and numSubPackets == 0:
            break
        subVersion, newIndex = parsePacket(binStr, idx)
        versionSum += subVersion

        if lengthTypeID == 0:
            subPacketLength -= newIndex - idx
        else:
            numSubPackets -= 1
        idx = newIndex
    return versionSum, idx

if __name__ == "__main__":
    with open("input.txt", "r") as file:
        hexStr = file.read().strip()

    binStr = hexToBin(hexStr)
    versionSum, _ = parsePacket(binStr, 0)
    print(versionSum)
