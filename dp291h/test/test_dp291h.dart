import 'package:test/test.dart';
import '../lib/dp291h.dart';

void main() {
    List<List<int>> description = [[6, 4],
                                   [5, 0, 4, 2],
                                   [1, 1, 5, 3],
                                   [0, 3, 4, 3]];
    group('Grid', () {
        test('Takes grid description can print grid.', () {
            Grid g = new Grid(description);
            expect(g.toString(), equals(
                    '.....A\n'
                  + '.B....\n'
                  + '....A.\n'
                  + 'C...CB'
                ));
        });
    });

    group('Path', () {
        test('given start, end, and grid, gives path', () {
            Grid g = new Grid(description);
            Path p = new Path([5, 0], [4, 2], g);
        });
    });
}
