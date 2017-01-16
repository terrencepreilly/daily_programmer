import 'package:test/test.dart';
import '../lib/map.dart';

void main() {
    group('load map', () {
        test('reads in a sequence of 0s and 1s', () {
            List<String> content = [
                '###',
                '#.#',
                '###',
                ];
            List<List<Node>> m = translate(content);
            expect(m[1][1].x, equals(1));
            expect(m[1][1].y, equals(1));
            expect(m[0][0], equals(null));
            expect(m[0][1], equals(null));
        });
    });

    group('Node', () {
        test('add', () {
            Node n1 = new Node(0, 0);
            Node n2 = new Node(0, 1);
            expect(n1.children.length, equals(0));
            expect(n2.children.length, equals(0));
            n1.add(n2);
            expect(
                n1.children.length, equals(1),
                reason: 'Caller\'s children didn\'t increment!'
                );
            expect(
                n2.children.length, equals(1),
                reason: 'Callee\'s children didn\'t increment!'
                );
            expect(n1.children.first, equals(n2));
            expect(n2.children.first, equals(n1));
        });
    });

    group('join', () {
        test('joins together each node to its neighbor', () {
            List<String> content = [
                '#.#',
                '#..',
                '##.',
                ];
            List<List<Node>> m = translate(content);
            List<List<Node>> joined = join(m);
            expect(joined[0][1].children.length, equals(1));
            expect(joined[0][1].children[0], equals(joined[1][1]));
        });
        test('Works with edge nodes', () {
            List<String> content = [
                '.##',
                '..#',
                '#.#',
                ];
            List<List<Node>> joined = join(translate(content));
            expect(joined[0][0].children.length, equals(1));
            expect(joined[1][0].children.length, equals(2));
            expect(joined[1][1].children.length, equals(2));
            expect(joined[2][1].children.length, equals(1));
        });
    });

    group('flatten', () {
        test('removes everything but nodes', () {
            List<String> content = [
                '#.#',
                '#..',
                '#.#',
                ];
            List<Node> nodes = flatten(join(translate(content)));
            expect(nodes.length, equals(4));
            expect(nodes[0].children.first, equals(nodes[1]));
        });
    });

    group('find', () {
        test('gets the node with the given index from the ordered list', () {
            List<String> content = [
                '.##',
                '..#',
                '#.#',
                ];
            List<Node> nodes = flatten(join(translate(content)));
            for (Node n in nodes) {
                expect(find(nodes, n.y, n.x, 3), equals(n));
            }
        });
    });

    group('DFS', () {
        test('finds a path to a node.', () {
            List<String> content = [
                '.##',
                '..#',
                '#.#',
                ];
            List<Node> nodes = flatten(join(translate(content)));
            expect(nodes.first.children.length, equals(1));
            List<Node> path = DFS(nodes, nodes.first, nodes.last);
            expect(path.length, equals(4));
            expect(path.first, equals(nodes.first));
            expect(path.last, equals(nodes.last));
        });
    });

    group('numbers', () {
        test('finds a list of numbers in the map', () {
            List<String> content = [
                '1..',
                '##2',
                '3#.',
                ];
            List<Node> nums = numbers(content);
            expect(nums.length, equals(3));
            expect(nums.first.x, equals(0));
            expect(nums.first.y, equals(0));
            expect(nums.last.x, equals(0));
            expect(nums.last.y, equals(2));
        });
        test('finds numbers in order', () {
            List<String> content = [
                '3..',
                '.#5',
                '..1',
                ];
            List<Node> nums = numbers(content);
            expect(nums.length, equals(3));
            expect(nums.first.x, equals(2));
            expect(nums.first.y, equals(2));
            expect(nums.last.x, equals(2));
            expect(nums.last.y, equals(1));
        });
    });
}
