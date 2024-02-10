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

func parsePacket(binStr string, idx int) (int, int, int64) {
	version := int(binStr[idx]-'0')<<2 | int(binStr[idx+1]-'0')<<1 | int(binStr[idx+2]-'0')
	typeID := int(binStr[idx+3]-'0')<<2 | int(binStr[idx+4]-'0')<<1 | int(binStr[idx+5]-'0')
	idx += 6

	if typeID == 4 {
		value := int64(0)
		for binStr[idx] == '1' {
			value = value<<4 | int64(binStr[idx+1]-'0')<<3 | int64(binStr[idx+2]-'0')<<2 | int64(binStr[idx+3]-'0')<<1 | int64(binStr[idx+4]-'0')
			idx += 5
		}
		value = value<<4 | int64(binStr[idx+1]-'0')<<3 | int64(binStr[idx+2]-'0')<<2 | int64(binStr[idx+3]-'0')<<1 | int64(binStr[idx+4]-'0')
		idx += 5
		return version, idx, value
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

	values := []int64{}
	for {
		if lengthTypeID == 0 && subPacketLength == 0 {
			break
		}
		if lengthTypeID == 1 && numSubPackets == 0 {
			break
		}
		_, newIndex, subValue := parsePacket(binStr, idx)
		values = append(values, subValue)

		if lengthTypeID == 0 {
			subPacketLength -= newIndex - idx
		} else {
			numSubPackets--
		}
		idx = newIndex
	}

	var result int64
	switch typeID {
	case 0:
		result = 0
		for _, value := range values {
			result += value
		}
	case 1:
		result = 1
		for _, value := range values {
			result *= value
		}
	case 2:
		result = values[0]
		for _, value := range values {
			if value < result {
				result = value
			}
		}
	case 3:
		result = values[0]
		for _, value := range values {
			if value > result {
				result = value
			}
		}
	case 5:
		result = 0
		if values[0] > values[1] {
			result = 1
		}
	case 6:
		result = 0
		if values[0] < values[1] {
			result = 1
		}
	case 7:
		result = 0
		if values[0] == values[1] {
			result = 1
		}
	default:
		panic("Unknown typeID")
	}

	return version, idx, result
}

func main() {
	data, err := os.ReadFile("input.txt")
	if err != nil {
		fmt.Println("Error reading file:", err)
		os.Exit(1)
	}

	hexStr := strings.TrimSpace(string(data))
	binStr := hexToBin(hexStr)
	_, _, value := parsePacket(binStr, 0)
	fmt.Printf("%d\n", value)
}