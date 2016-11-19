import 'package:test/test.dart';
import '../lib/dp283h.dart';

int absval(int i) {
    if (i < 0)
        return -1 * i;
    return i;
}

void main() {
    String filename = './test/test_map1.txt';
    group('loadMap()', () {
        test('returns a list of strings.', () async {
            expect(await loadMap(filename), equals(const [
                ' **  ',
                '*  **',
                '*  * ',
                ' **  ',
                ])
            );
        });
    });

    group('convertToInts()', () {
        test('converts a list of strings into a list of lists of ints', () async {
            List<String> data = await loadMap(filename);
            expect(convertToInts(data), equals(const [
                const [0, 1, 1, 0, 0],
                const [1, 0, 0, 1, 1],
                const [1, 0, 0, 1, 0],
                const [0, 1, 1, 0, 0],
                ])
            );
        });
    });

    group('Point', () {
        test('compareTo() compares ys then xs', () {
            Point p0 = new Point(0, 0);
            Point p1 = new Point(1, 0);
            Point p2 = new Point(0, 1);
            expect(p0.compareTo(p0), equals(0));
            expect(p0.compareTo(p1), lessThan(0));
            expect(p0.compareTo(p2), lessThan(0));
        });
    });

    group('between()', () {
        test('returns a function that checks if it is between', () {
            Function b = between(0, 10);
            expect(b(5), equals(true));
            expect(b(-1), equals(false));
            expect(b(10), equals(false));
        });
    });

    group('findOutlier()', () {
        is_one_of(Point p, List<Point> points) {
            for (Point pt in points)
                if (p == pt)
                    return true;
            return false;
        }

        test('can find outliers from the north direction', () async {
            List<List<int>> data = convertToInts(await loadMap(filename));
            List<Point> outliers = findOutliers(data, 'N');

            for (Point outlier in outliers)
                expect(
                    is_one_of(outlier, [new Point(1, 0), new Point(2, 0)]),
                    equals(true)
                );
        });

        test('can find outliers from the south direction', () async {
            List<List<int>> data = convertToInts(await loadMap(filename));
            List<Point> outliers = findOutliers(data, 'S');
            for (Point outlier in outliers)
                expect(
                    is_one_of(outlier, [new Point(1, 3), new Point(2, 3)]),
                    equals(true)
                );
        });

        test('can find outliers from the eastern direction', () async {
            List<List<int>> data = convertToInts(await loadMap(filename));
            List<Point> outliers = findOutliers(data, 'E');
            for (Point outlier in outliers)
                expect(
                    is_one_of(outlier, [new Point(0, 1), new Point(0, 2)]),
                    equals(true)
                );
        });

        test('can find outliers from the western direction', () async {
            List<List<int>> data = convertToInts(await loadMap(filename));
            List<Point> outliers = findOutliers(data, 'W');
        });
    });
}
