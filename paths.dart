// Copyright 2016 Google Inc. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

class Path {}

class Cons implements Path {
  final head;
  final tail;

  Cons(this.head, this.tail);

  toString() => "$head:$tail";
}

class Nil implements Path {
  toString() => "[]";
}

class Tree {
  final value;
  final children;

  Tree(this.value, this.children);
}

class StackFrame {
  List<Tree> children;
  int index;

  StackFrame(this.children, this.index);
}

List<Path> paths(Tree tree) {
  final result = [];
  final stack = [new StackFrame(tree.children, -1)];
  var prefix = new Cons(tree.value, new Nil());
  while (!stack.isEmpty) {
    final frame = stack.last;
    frame.index++;
    final children = frame.children;
    final index = frame.index;
    if (index < children.length) {
      final child = children[index];
      prefix = new Cons(child.value, prefix);
      stack.add(new StackFrame(child.children, -1));
    } else {
      if (children.isEmpty) {
        result.add(prefix);
      }
      prefix = prefix.tail;
      stack.removeLast();
    }
  }
  return result;
}

main() {
  final tree =
    new Tree("a", [
      new Tree("b", [
        new Tree("c", []),
        new Tree("d", [])]),
      new Tree("e", [])
    ]);
  var t = new Tree("t1", [tree, tree]);
  for (var i=0; i < 20; i++) {
  t = new Tree("foo", [t, t]);
  }
  print(paths(t));
}
