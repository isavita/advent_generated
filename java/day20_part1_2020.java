
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class JurassicJigsaw {

    static class Tile {
        int id;
        char[][] data;
        String[] borders;

        public Tile(int id, char[][] data) {
            this.id = id;
            this.data = data;
            this.borders = calculateBorders();
        }

        private String[] calculateBorders() {
            String[] borders = new String[8];
            int n = data.length;

            StringBuilder top = new StringBuilder();
            StringBuilder bottom = new StringBuilder();
            StringBuilder left = new StringBuilder();
            StringBuilder right = new StringBuilder();

            for (int i = 0; i < n; i++) {
                top.append(data[0][i]);
                bottom.append(data[n - 1][i]);
                left.append(data[i][0]);
                right.append(data[i][n - 1]);
            }

            borders[0] = top.toString();
            borders[1] = right.toString();
            borders[2] = bottom.toString();
            borders[3] = left.toString();
            borders[4] = top.reverse().toString();
            borders[5] = right.reverse().toString();
            borders[6] = bottom.reverse().toString();
            borders[7] = left.reverse().toString();

            return borders;
        }
    }

    public static void main(String[] args) {
        List<Tile> tiles = readTiles("input.txt");
        Map<String, List<Tile>> borderMap = buildBorderMap(tiles);
        long cornerProduct = findCornerProduct(tiles, borderMap);

        System.out.println(cornerProduct);
    }

    static List<Tile> readTiles(String filename) {
        List<Tile> tiles = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            String line;
            int id = 0;
            List<String> tileData = new ArrayList<>();
            while ((line = br.readLine()) != null) {
                if (line.startsWith("Tile")) {
                    id = Integer.parseInt(line.substring(5, line.length() - 1));
                } else if (line.isEmpty()) {
                    if (!tileData.isEmpty()) {
                        tiles.add(createTile(id, tileData));
                        tileData.clear();
                    }
                } else {
                    tileData.add(line);
                }
            }
            if (!tileData.isEmpty()) {
                tiles.add(createTile(id, tileData));
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return tiles;
    }

    static Tile createTile(int id, List<String> tileData) {
        int n = tileData.size();
        char[][] data = new char[n][n];
        for (int i = 0; i < n; i++) {
            data[i] = tileData.get(i).toCharArray();
        }
        return new Tile(id, data);
    }

    static Map<String, List<Tile>> buildBorderMap(List<Tile> tiles) {
        Map<String, List<Tile>> borderMap = new HashMap<>();
        for (Tile tile : tiles) {
            for (String border : tile.borders) {
                borderMap.computeIfAbsent(border, k -> new ArrayList<>()).add(tile);
            }
        }
        return borderMap;
    }

    static long findCornerProduct(List<Tile> tiles, Map<String, List<Tile>> borderMap) {
        long product = 1;
        for (Tile tile : tiles) {
            int uniqueBorders = 0;
            for (int i = 0; i < 4; i++) {
                if (borderMap.get(tile.borders[i]).size() == 1) {
                    uniqueBorders++;
                }
            }
            if (uniqueBorders == 2) {
                product *= tile.id;
            }
        }
        return product;
    }
}
