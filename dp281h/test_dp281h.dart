import 'package:test/test.dart';

import 'dp281h.dart';

main() {
    const List<String> field = const [
        ' 1???',
        ' 11??',
        '  2??',
        '  111',
        ];

    test('all_numbers() returns all of the numbers', () {
        Field f = new Field(field);
        List<Point> nums = new List<Point>();
        for (Point p in all_numbers(f))
            nums.add(p);
        expect(nums.length, equals(7));
        expect(nums[0].score, equals(1));
        expect(nums[3].score, equals(2));
    });

    group('Field', () {
        Field f = new Field(field);

        test('Field() creates a new Field with same length as f', () {
            expect(f.length, equals(field.length));
        });

        test('Field() puts space points where spaces were', () {
            for (Point p in [new Point(0, 0),
                             new Point(1, 0),
                             new Point(2, 0),
                             new Point(3, 0),
                             new Point(2, 1),
                             new Point(3, 1)]) {
                PointType pt = f.query(p).ptype;
                expect(pt, equals(PointType.space));
            }
        });

        test('Field() puts numbers where they are', () {
            for (List<int> p in [[0, 1, 1], [1, 1, 1], [1, 2, 1],
                                 [2, 2, 2], [3, 2, 1], [3, 3, 1],
                                 [3, 4, 1]]) {
                Point point = f.query(new Point(p[0], p[1]));
                expect(point.ptype, equals(PointType.number));
                expect(point.score, equals(p[2]));
            }
        });

        test('Field() puts questiens where they are', () {
            expect(f.query(new Point(0, 3)).ptype,
                   equals(PointType.question));
            expect(f.query(new Point(2, 4)).ptype,
                   equals(PointType.question));
        });

        test('get() returns the point at the query', () {
            for (int x = 0; x < f.length; x++) {
                for (int y = 0; y < f.width; y++) {
                    Point query = new Point(x, y);
                    expect(f.query(query), equals(f.field[x][y]));
                }
            }
        });

    });

    test('questions() returns the question marks around p', () {
        Field f = new Field(field);
        for (List<int> p in [[1, 2, 4], [0, 1, 1], [3, 4, 2]]) {
            List<Point> qs = questions(f, new Point(p[0], p[1]));
            expect(qs.length, equals(p[2]));
        }
    });

    group('is_valid()', () {
        Field f = new Field(field);

        test('returns true if point is near p', () {
            Point p = new Point(1, 1);
            for (int i = 0; i <= 2; i++) {
                for (int j = 0; j <= 2; j++) {
                    if (i == 1 && j == 1)
                        continue;
                    expect(is_valid(f, p, i, j), equals(true));
                }
            }
        });

        test('returns false if point is not near p', () {
            Point p = new Point(0, 0);
            expect(is_valid(f, p, p.x+2, p.y+2), equals(false));
            expect(is_valid(f, p, p.x+2, p.y), equals(false));
        });

        test('returns false if point is outside of field', () {
            Point p = new Point(0, 0);
            for (int i = -1; i < 2; i++) {
                expect(is_valid(f, p, -1, i), equals(false));
                expect(is_valid(f, p, i, -1), equals(false));
            }
        });

        test('returns false if point is at p', () {
            Point p = new Point(0, 0);
            expect(is_valid(f, p, p.x, p.y), equals(false));
        });
    });

    group('Point', () {
        test('Point() creates a point with x and y', () {
            Point p = new Point(1, 3, PointType.mine);
            expect(p.x, equals(1));
            expect(p.y, equals(3));
        });

        test('decrement() lowers score', () {
            Point p = new Point(0, 4, PointType.number, 3);
            expect(p.score, equals(3));
            p.decrement();
            expect(p.score, equals(2));
        });
    });


    test('scan() finds index of mine, if any', () {
        Field f = new Field(field);
        Point p = f.query(new Point(0, 1));
        List<Point> pts = scan(f, p);
        expect(pts[0].x, equals(0));
        expect(pts[0].y, equals(2));
    });

    test('flag() changes questions to mines, decrements numbers', () {
        Field f = new Field(field);
        Point p = f.query(new Point(0, 1));
        List<Point> qs = scan(f, p);
        for (Point q in qs)
            flag(f, q);
        for (Point q in qs)
            expect(q.ptype, equals(PointType.mine));
    });
}
