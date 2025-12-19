
import java.util.*;
import java.io.*;
import java.util.regex.*;

public class Main {
    static int bestResult, rank, numButtons;
    static int[] freeValues, maxPresses, pivotCol;
    static List<Integer> freeVars;
    static double[][] matrix;

    public static void main(String[] args) throws Exception {
        Scanner sc = new Scanner(new File("input.txt"));
        int total = 0;
        while (sc.hasNextLine()) {
            String line = sc.nextLine().trim();
            if (line.isEmpty()) continue;
            List<int[]> buttons = new ArrayList<>();
            Matcher bm = Pattern.compile("\\(([^)]*)\\)").matcher(line);
            while (bm.find()) {
                String s = bm.group(1).trim();
                if (s.isEmpty()) { buttons.add(new int[0]); continue; }
                String[] pts = s.split(",");
                int[] b = new int[pts.length];
                for (int i = 0; i < pts.length; i++) b[i] = Integer.parseInt(pts[i].trim());
                buttons.add(b);
            }
            Matcher tm = Pattern.compile("\\{([^}]*)\\}").matcher(line);
            if (tm.find()) {
                String[] pts = tm.group(1).split(",");
                int[] targets = new int[pts.length];
                for (int i = 0; i < pts.length; i++) targets[i] = Integer.parseInt(pts[i].trim());
                total += solve(buttons, targets);
            }
        }
        System.out.println(total);
    }

    static int solve(List<int[]> buttons, int[] targets) {
        int n = targets.length;
        numButtons = buttons.size();
        matrix = new double[n][numButtons + 1];
        for (int j = 0; j < n; j++) matrix[j][numButtons] = targets[j];
        for (int i = 0; i < numButtons; i++) for (int j : buttons.get(i)) if (j < n) matrix[j][i] = 1;
        pivotCol = new int[n];
        Arrays.fill(pivotCol, -1);
        int r = 0;
        for (int c = 0; c < numButtons && r < n; c++) {
            int mr = r;
            for (int i = r + 1; i < n; i++) if (Math.abs(matrix[i][c]) > Math.abs(matrix[mr][c])) mr = i;
            if (Math.abs(matrix[mr][c]) < 1e-9) continue;
            double[] tmp = matrix[r]; matrix[r] = matrix[mr]; matrix[mr] = tmp;
            double s = matrix[r][c];
            for (int k = c; k <= numButtons; k++) matrix[r][k] /= s;
            for (int i = 0; i < n; i++) if (i != r && Math.abs(matrix[i][c]) > 1e-9) {
                double f = matrix[i][c];
                for (int k = c; k <= numButtons; k++) matrix[i][k] -= f * matrix[r][k];
            }
            pivotCol[r++] = c;
        }
        rank = r;
        for (int i = rank; i < n; i++) if (Math.abs(matrix[i][numButtons]) > 1e-9) return -1;
        boolean[] isP = new boolean[numButtons];
        for (int i = 0; i < rank; i++) if (pivotCol[i] >= 0) isP[pivotCol[i]] = true;
        freeVars = new ArrayList<>();
        for (int i = 0; i < numButtons; i++) if (!isP[i]) freeVars.add(i);
        maxPresses = new int[numButtons];
        for (int i = 0; i < numButtons; i++) {
            int m = Integer.MAX_VALUE;
            for (int j : buttons.get(i)) if (j < n) m = Math.min(m, targets[j]);
            maxPresses[i] = (m == Integer.MAX_VALUE) ? 0 : m;
        }
        freeVars.sort(Comparator.comparingInt(i -> maxPresses[i]));
        bestResult = Integer.MAX_VALUE;
        freeValues = new int[freeVars.size()];
        enumerate(0, 0);
        return bestResult == Integer.MAX_VALUE ? -1 : bestResult;
    }

    static void enumerate(int idx, int sum) {
        if (sum >= bestResult) return;
        if (idx == freeVars.size()) {
            int[] res = new int[numButtons];
            for (int i = 0; i < freeVars.size(); i++) res[freeVars.get(i)] = freeValues[i];
            for (int i = rank - 1; i >= 0; i--) {
                int c = pivotCol[i];
                if (c < 0) continue;
                double v = matrix[i][numButtons];
                for (int k = c + 1; k < numButtons; k++) v -= matrix[i][k] * res[k];
                int iv = (int) Math.round(v);
                if (Math.abs(v - iv) > 1e-6 || iv < 0 || iv > maxPresses[c]) return;
                res[c] = iv;
            }
            int cur = 0;
            for (int p : res) cur += p;
            if (cur < bestResult) bestResult = cur;
        } else {
            int fv = freeVars.get(idx);
            for (int v = 0; v <= maxPresses[fv]; v++) {
                freeValues[idx] = v;
                enumerate(idx + 1, sum + v);
            }
        }
    }
}
