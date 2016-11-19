import "package:test/test.dart";
import 'dart:async';

import "dp281i.dart";

main() async {
    group('dart', () {
        test('list contents', () {
            const List<String> a = const ['a', 'b', 'c'];
            expect(a.contains('a'), equals(true));
            expect(a.contains('d'), equals(false));
        });
    });

    group('ordered_subsets()', () {
        test('returns the ordered subsets, largest first', () {
            List<String> expected =
                ['cat', 'at', 'ct', 'ca', 't', 'a', 'c'];

            expect(ordered_subsets('cat'), equals(expected));
        });
    });

    group('read_file()', () {
        test('reads example.txt and returns the first lines', () async {
            List<String> words = new List();
            List<String> expected = ['line1', 'line2', 'line3'];
            await read_file(words, 'example.txt');
            expect(words, equals(expected));
        });
    });
}
