String ALPH = '.ABCDEFGHIJKLMNOPQRSTUVWXYZ';


class Grid {
    List<List<int>> grid;

    Grid(List<List<int>> description) {
        grid = new List<List<int>>();
        List<int> dim = description[0];
        for (int y = 0; y < dim[1]; y++) {
            this.grid.add(new List<int>.filled(dim[0], 0));
        }
        for (int i = 1; i < description.length; i++) {
            List<int> start = [description[i][0], description[i][1]];
            List<int> end = [description[i][2], description[i][3]];
            this.grid[start[1]][start[0]] = i;
            this.grid[end[1]][end[0]] = i;
        }
    }

    String toString() {
        return this.grid.map((y) =>
            y.map((x) => ALPH[x]).join('')
        ).join('\n');
    }
}


class Path {
    List<int> start;
    List<int> stop;
    Grid grid;

    Path(this.start, this.stop, this.grid);
}


void main() {
    List<List<int>> description = [
        [6, 4],
        [5, 0, 4, 2],
        [1, 1, 5, 3],
        [0, 3, 4, 3]
    ];
    Grid g = new Grid(description);
    print(g.toString());
}
