
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

public class Main {
    public static void main(String[] args) throws IOException {
        String input = new String(Files.readAllBytes(Paths.get("input.txt"))).trim();
        System.out.println(solve(input));
    }

    static int solve(String input) {
        List<Tile> tiles = parseTilesFromInput(input);
        int edgeSize = (int) Math.sqrt(tiles.size());
        Tile[][] assembledTiles = backtrackAssemble(tiles, new Tile[edgeSize][edgeSize], new boolean[tiles.size()]);

        for (Tile[] row : assembledTiles) {
            for (Tile tile : row) {
                tile.contents = removeBordersFromGrid(tile.contents);
            }
        }

        List<List<String>> image = new ArrayList<>();
        for (int bigRow = 0; bigRow < edgeSize; bigRow++) {
            for (int subRow = 0; subRow < assembledTiles[0][0].contents.size(); subRow++) {
                List<String> newRow = new ArrayList<>();
                for (int bigCol = 0; bigCol < edgeSize; bigCol++) {
                    newRow.addAll(assembledTiles[bigRow][bigCol].contents.get(subRow));
                }
                image.add(newRow);
            }
        }

        List<int[]> monsterCoords = new ArrayList<>();
        for (List<List<String>> opt : allGridOrientations(image)) {
            monsterCoords = findMonsterCoords(opt);
            if (!monsterCoords.isEmpty()) {
                image = opt;
                break;
            }
        }

        for (int[] coord : monsterCoords) {
            image.get(coord[0]).set(coord[1], "O");
        }

        int roughWatersCount = 0;
        for (List<String> row : image) {
            for (String cell : row) {
                if (cell.equals("#")) roughWatersCount++;
            }
        }

        return roughWatersCount;
    }

    static List<Tile> parseTilesFromInput(String input) {
        List<Tile> tiles = new ArrayList<>();
        String[] blocks = input.split("\n\n");
        for (String block : blocks) {
            String[] split = block.split("\n");
            int tileID = Integer.parseInt(split[0].substring(5, split[0].length() - 1));
            List<List<String>> contents = new ArrayList<>();
            for (int i = 1; i < split.length; i++) {
                contents.add(Arrays.asList(split[i].split("")));
            }
            tiles.add(new Tile(tileID, contents));
        }
        return tiles;
    }

    static Tile[][] backtrackAssemble(List<Tile> tiles, Tile[][] assembledTiles, boolean[] usedIndices) {
        int edgeSize = (int) Math.sqrt(tiles.size());
        for (int row = 0; row < edgeSize; row++) {
            for (int col = 0; col < edgeSize; col++) {
                if (assembledTiles[row][col] == null) {
                    for (int i = 0; i < tiles.size(); i++) {
                        if (!usedIndices[i]) {
                            for (List<List<String>> opt : allGridOrientations(tiles.get(i).contents)) {
                                if (row > 0 && !getRow(opt, true).equals(getRow(assembledTiles[row - 1][col].contents, false))) continue;
                                if (col > 0 && !getCol(opt, true).equals(getCol(assembledTiles[row][col - 1].contents, false))) continue;

                                assembledTiles[row][col] = new Tile(tiles.get(i).id, opt);
                                usedIndices[i] = true;

                                if (backtrackAssemble(tiles, assembledTiles, usedIndices) != null) return assembledTiles;

                                assembledTiles[row][col] = null;
                                usedIndices[i] = false;
                            }
                        }
                    }
                    return null;
                }
            }
        }
        return assembledTiles;
    }

    static String getCol(List<List<String>> grid, boolean firstCol) {
        StringBuilder str = new StringBuilder();
        for (List<String> strings : grid) {
            str.append(firstCol ? strings.get(0) : strings.get(strings.size() - 1));
        }
        return str.toString();
    }

    static String getRow(List<List<String>> grid, boolean firstRow) {
        StringBuilder str = new StringBuilder();
        for (int i = 0; i < grid.get(0).size(); i++) {
            str.append(firstRow ? grid.get(0).get(i) : grid.get(grid.size() - 1).get(i));
        }
        return str.toString();
    }

    static List<List<String>> removeBordersFromGrid(List<List<String>> grid) {
        List<List<String>> result = new ArrayList<>();
        for (int i = 1; i < grid.size() - 1; i++) {
            List<String> newRow = new ArrayList<>();
            for (int j = 1; j < grid.get(0).size() - 1; j++) {
                newRow.add(grid.get(i).get(j));
            }
            result.add(newRow);
        }
        return result;
    }

    static final String MONSTER = "                  # \n#    ##    ##    ###\n #  #  #  #  #  #   ";

    static List<int[]> findMonsterCoords(List<List<String>> image) {
        List<int[]> monsterOffsets = new ArrayList<>();
        String[] monsterLines = MONSTER.split("\n");
        for (int r = 0; r < monsterLines.length; r++) {
            for (int c = 0; c < monsterLines[r].length(); c++) {
                if (monsterLines[r].charAt(c) == '#') {
                    monsterOffsets.add(new int[]{r, c});
                }
            }
        }

        List<int[]> monsterStartingCoords = new ArrayList<>();
        for (int r = 0; r <= image.size() - monsterLines.length; r++) {
            for (int c = 0; c <= image.get(0).size() - monsterLines[0].length(); c++) {
                boolean monsterFound = true;
                for (int[] offset : monsterOffsets) {
                    if (!image.get(r + offset[0]).get(c + offset[1]).equals("#")) {
                        monsterFound = false;
                        break;
                    }
                }
                if (monsterFound) {
                    monsterStartingCoords.add(new int[]{r, c});
                }
            }
        }

        List<int[]> monsterCoords = new ArrayList<>();
        for (int[] startingCoord : monsterStartingCoords) {
            for (int[] offset : monsterOffsets) {
                monsterCoords.add(new int[]{startingCoord[0] + offset[0], startingCoord[1] + offset[1]});
            }
        }

        return monsterCoords;
    }

    static List<List<List<String>>> allGridOrientations(List<List<String>> grid) {
        List<List<List<String>>> orientations = new ArrayList<>();
        orientations.add(grid);
        for (int i = 0; i < 3; i++) {
            orientations.add(rotateStringGrid(orientations.get(orientations.size() - 1)));
        }
        for (int i = 0; i < 4; i++) {
            orientations.add(mirrorStringGrid(orientations.get(i)));
        }
        return orientations;
    }

    static List<List<String>> rotateStringGrid(List<List<String>> grid) {
        int n = grid.size();
        List<List<String>> rotated = new ArrayList<>(n);
        for (int i = 0; i < n; i++) {
            rotated.add(new ArrayList<>(Collections.nCopies(n, "")));
        }
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                rotated.get(n - 1 - j).set(i, grid.get(i).get(j));
            }
        }
        return rotated;
    }

    static List<List<String>> mirrorStringGrid(List<List<String>> grid) {
        List<List<String>> flipped = new ArrayList<>();
        for (List<String> row : grid) {
            List<String> newRow = new ArrayList<>(row);
            Collections.reverse(newRow);
            flipped.add(newRow);
        }
        return flipped;
    }

    static class Tile {
        List<List<String>> contents;
        int id;

        Tile(int id, List<List<String>> contents) {
            this.id = id;
            this.contents = contents;
        }
    }
}
