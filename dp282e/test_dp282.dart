import 'package:test/test.dart';
import 'dp282.dart';

main() {
    group('fib()', () {
        test('Given index, gives list of that many fib. numbers', () {
            expect(fib(7), equals([13, 8, 5, 3, 2, 1, 1]));
        });
    });

    group('parse_line()', () {
        test('Takes a line, returns the number in base 10', () {
            List lines = [['10 16', 16],
                          ['16 ff', 255],
                          ['3 102', 11]];
            for (var line in lines) {
                expect(parse_line(line[0]), equals(line[1]));
            }
        });
    });

    group('max_less_than()', () {
        test('Returns the max number in l less than or equal to m', () {
            expect(max_less_than(10, [8, 5, 3]), equals(8));
            expect(max_less_than(10, [11, 9, 4]), equals(9));
            expect(max_less_than(10, [10, 3, 0, -1]), equals(10));
        });
    });

    group('zeros_except()', () {
        test('returns a list of size n with all zeros except i', () {
            expect(zeros_except(3, 1), equals([0, 1, 0]));
            expect(zeros_except(3, 0), equals([1, 0, 0]));
            expect(zeros_except(4, 3), equals([0, 0, 0, 1]));
        });
    });

    group('to_fib()', () {
        List<int> f = fib(7);
        test('Takes a number in base 10 and translates to Base Fib.', () {
            List expected = [[16, [1, 0, 0, 1, 0, 0, 0]],
                             [10, [0, 1, 0, 0, 1, 0, 0]]];
            for (List e in expected)
                expect(to_fib(f, e[0]), equals(e[1]));
        });
        test('Returns largest fib. number if equal to dec.', () {
            expect(to_fib(f, 13), equals([1, 0, 0, 0, 0, 0, 0]));
            expect(to_fib(f, 8), equals([0, 1, 0, 0, 0, 0, 0]));
        });
    });

    group('piecewise_or()', () {
        test('returns a list with or over its joined members', () {
            expect(piecewise_or([1, 0, 1], [0, 1, 0]), equals([1, 1, 1]));
            expect(piecewise_or([1, 1, 1], [1, 1, 1]), equals([1, 1, 1]));
            expect(piecewise_or([0, 0, 0], [0, 0, 0]), equals([0, 0, 0]));
            expect(piecewise_or([0, 1, 0], [0, 0, 0]), equals([0, 1, 0]));
        });
    });
}
