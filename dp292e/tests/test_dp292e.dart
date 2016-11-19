import 'package:test/test.dart';
import 'package:dp292e/dp292e.dart';

void main() {
    group('Digit', () {
        group('force_up()', () {
            test('returns parse when lower', () {
                Digit d = new Digit('5');
                expect(d.force_up(new Digit('4')), equals(5));
            });

            test('returns ten higher when same rank', () {
                Digit d = new Digit('5');
                expect(d.force_up(new Digit('5')), equals(15));

                expect(d.force_up(new Digit('6')), equals(15));
            });

            test('returns significant plus lower when different rank', () {
                Digit d = new Digit('5');
                expect(d.force_up(new Digit('40')), equals(45));
            });

            test('handles 0 as significant', () {
                Digit d = new Digit('0');
                expect(d.force_up(new Digit('5')), equals(10));

                expect(new Digit('02').force_up(new Digit('104')), equals(202));
            });
        });
    });

    group('Range', () {
    });
}
