import 'package:test/test.dart';

import 'dp281e.dart';

main() {
    group('transliterate()', () {
        test('returns string repr. of a number', () {
            expect(transliterate(8), equals('8'));
            expect(transliterate(0), equals('0'));
        });

        test('handles digit sup to \'f\'', () {
            expect(transliterate(15), equals('f'));
            expect(transliterate(10), equals('a'));
        });

        test('returns a number for a string', () {
            expect(transliterate('a'), equals(10));
            expect(transliterate('0'), equals(0));
            expect(transliterate('1'), equals(1));
        });
    });

    group('highest()', () {
        test('returns highest single digit in a string', () {
            expect(highest('1'), equals(1));
            expect(highest('12345'), equals(5));
            expect(highest('938573'), equals(9));
            expect(highest('353975'), equals(9));
            expect(highest('37377'), equals(7));
        });

        test('handles digits up to \'f\'', () {
            expect(highest('f'), equals(15));
            expect(highest('3258a83'), equals(10));
        });

    });

    group('incr()', () {
        test('returns one more than the number', () {
            int i = -10;
            while (i < 10)
                expect(incr(i), equals(++i));
        });
    });

    group('sum()', () {
        test('returns the sum of the list', () {
            expect(sum([1, 2, 3, 4, 5]), equals(15));
            expect(sum([-10, 10]), equals(0));
            expect(sum([1, 10, 100, 1000]), equals(1111));
        });
    });

    group('zip()', () {
        test('returns the two lists zipped', () {
            for (List<int> l in zip([1, 2, 3], [1, 2, 3]))
                expect(l[0], equals(l[1]));
        });
    });

    group('translate()', () {
        test('returns base-10 representation of base < 10', () {
            expect(translate(2, [1, 0, 1]), equals(5));
            expect(translate(8, [1, 7]), equals(15));
            expect(translate(9, [8, 8]), equals(80));
        });
        test('returns base-10 representation of base-10', () {
            expect(translate(10, [1, 3, 3, 5]), equals(1335));
        });
        test('returns base-10 representation of base > 10', () {
            expect(translate(16, [15, 15, 15]), equals(4095));
            expect(translate(12, [3, 7, 11, 8]), equals(6332));
        });
    });

    group('translate_word()', () {
        test('translates the base of a number', () {
            for (List x in [['1', 1],
                            ['21', 7],
                            ['ab3', 1575],
                            ['ff', 255]])
                expect(translate_word(x[0]), equals(x[1]));
        });
    });
}
