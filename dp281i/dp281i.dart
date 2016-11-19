import 'dart:math';
import 'dart:io';
import 'dart:convert';
import 'dart:async';

Iterable<String> ordered_subsets(String A) sync* {
    List<String> q = new List();
    Set<String> s = new Set();
    q.add(A);
    s.add(A);
    while (q.length > 0) {
        String curr = q.removeAt(0);
        if (curr.length == 0)
            continue;
        for (int i = 0; i < curr.length; i++) {
            String tmp = curr.substring(0, i) +
                         curr.substring(i+1, curr.length);
            if (! s.contains(tmp)) {
                q.add(tmp);
                s.add(tmp);
            }
        }
        yield curr;
    }
}

Future read_file(List<String> words, String filename) async {
    Stream<List<int>> stream = new File(filename).openRead();
    return stream
        .transform(UTF8.decoder)
        .transform(const LineSplitter())
        .listen((line) {
            words.add(line);
        }).asFuture().catchError((_) => stderr.writeln('problem'));
}

main(List<String> arguments) async {
    List<String> words = new List();
    await read_file(words, 'enable1.txt');
    for (String username in ordered_subsets(arguments[0]?.toLowerCase())) {
        if (words.contains(username)) {
            print(username);
            exit(0);
        }
    }
}
