import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;

public class Main {
    static class IPRange implements Comparable<IPRange> {
        long start;
        long end;

        IPRange(long start, long end) {
            this.start = start;
            this.end = end;
        }

        @Override
        public int compareTo(IPRange o) {
            return Long.compare(this.start, o.start);
        }
    }

    public static void main(String[] args) throws IOException {
        IPRange[] ipRanges = readIPRanges("input.txt");
        Arrays.sort(ipRanges);
        long unblockedIP = findUnblockedIP(ipRanges);
        System.out.println(unblockedIP);
    }

    static IPRange[] readIPRanges(String filename) throws IOException {
        BufferedReader br = new BufferedReader(new FileReader(filename));
        String line;
        int count = 0;
        while ((line = br.readLine()) != null) {
            count++;
        }
        br.close();

        br = new BufferedReader(new FileReader(filename));
        IPRange[] ranges = new IPRange[count];
        for (int i = 0; (line = br.readLine()) != null; i++) {
            String[] parts = line.split("-");
            long start = Long.parseLong(parts[0]);
            long end = Long.parseLong(parts[1]);
            ranges[i] = new IPRange(start, end);
        }
        br.close();
        return ranges;
    }

    static long findUnblockedIP(IPRange[] ranges) {
        long currentIP = 0;
        for (IPRange r : ranges) {
            if (r.start > currentIP) {
                return currentIP;
            }
            if (r.end >= currentIP) {
                currentIP = r.end + 1;
            }
        }
        return currentIP;
    }
}