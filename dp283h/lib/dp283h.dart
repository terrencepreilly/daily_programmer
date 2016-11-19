import 'dart:io';
import 'dart:async';

class Point implements Comparable {
    final int x;
    final int y;
    Point(this.x, this.y);

    int compareTo(Point other) {
        if (this.y.compareTo(other.y) != 0)
            return this.y.compareTo(other.y);
        return this.x.compareTo(other.x);
    }

    bool operator==(Point other) {
        return this.compareTo(other) == 0;
    }
}

Future<String> loadMap(String filename) async {
    return await new File(filename).readAsLines();
}

List<List<int>> convertToInts(List<String> data) {
    List<List<int>> ret = new List<List<int>>();
    for (String s in data) {
        ret.add(new List<int>.generate(s.length, (x) => s[x] == '*' ? 1 : 0));
    }
    return ret;
}

bool between(lower, upper) {
    return (x) => lower < x && x < upper;
}

List<Point> findOutliers(List<List<int>> data, String direction) {
    List<Point> ret = new List<Point>();
    bool cont = true;

    int ystart = direction == 'N' ? 0 : data.length - 1;
    int xstart = direction == 'W' ? data[0].length - 1 : 0;
    Function ycond = between(-1, data.length);
    Function xcond = between(-1, data[0].length);
    Function yupdate = direction == 'N' ? (y) => y+1 : (y) => y-1;
    Function xupdate = direction == 'W' ? (y) => y-1 : (y) => y+1;

    if (direction == 'N' || direction == 'S') {
        for (int y = ystart; ycond(y) && cont; y = yupdate(y)) {
            for (int x = xstart; xcond(x); x = xupdate(x)) {
                if (data[y][x] == 1) {
                    cont = false;
                    ret.add(new Point(x, y));
                }
            }
        }
    } else {
        for (int x = xstart; xcond(x); x = xupdate(x)) {
            for (int y = ystart; ycond(y) && cont; y = yupdate(y)) {
                if (data[y][x] == 1) {
                    cont = false;
                    ret.add(new Point(x, y));
                }
            }
        }
    }
    return ret;
}

void main(List<String> args) async {

}
