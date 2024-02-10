package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func hexToBin(hex string) string {
	bin := ""
	for _, h := range hex {
		b, _ := strconv.ParseUint(string(h), 16, 8)
		bin += fmt.Sprintf("%04b", b)
	}
	return bin
}

func parsePacket(binStr string, idx int) (int, int) {
	version := int(binStr[idx]-'0')<<2 | int(binStr[idx+1]-'0')<<1 | int(binStr[idx+2]-'0')
	typeID := int(binStr[idx+3]-'0')<<2 | int(binStr[idx+4]-'0')<<1 | int(binStr[idx+5]-'0')
	idx += 6

	if typeID == 4 {
		for binStr[idx] == '1' {
			idx += 5
		}
		idx += 5
		return version, idx
	}

	lengthTypeID := int(binStr[idx] - '0')
	idx++
	var numSubPackets, subPacketLength int

	if lengthTypeID == 0 {
		subPacketLength = 0
		for i := 0; i < 15; i++ {
			subPacketLength = subPacketLength<<1 | int(binStr[idx]-'0')
			idx++
		}
	} else {
		numSubPackets = 0
		for i := 0; i < 11; i++ {
			numSubPackets = numSubPackets<<1 | int(binStr[idx]-'0')
			idx++
		}
	}

	versionSum := version
	for {
		if lengthTypeID == 0 && subPacketLength == 0 {
			break
		}
		if lengthTypeID == 1 && numSubPackets == 0 {
			break
		}
		subVersion, newIndex := parsePacket(binStr, idx)
		versionSum += subVersion

		if lengthTypeID == 0 {
			subPacketLength -= newIndex - idx
		} else {
			numSubPackets--
		}
		idx = newIndex
	}
	return versionSum, idx
}

func main() {
	data, err := os.ReadFile("input.txt")
	if err != nil {
		fmt.Println("Error reading file:", err)
		os.Exit(1)
	}

	hexStr := strings.TrimSpace(string(data))
	binStr := hexToBin(hexStr)
	versionSum, _ := parsePacket(binStr, 0)
	fmt.Println(versionSum)
}