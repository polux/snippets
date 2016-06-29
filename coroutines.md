<!--
 Copyright 2016 Google Inc. All Rights Reserved.

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http:www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
-->

# Encoding Coroutines

We show how to encode coroutines in a language that doesn't have them. Here
we use Dart.

## Coroutines

We would like to add a `pause` construct to our programming language (here
Dart). The meaning of `pause` would be: "yield back control to the scheduler and
wake me up later".

~~~{.dart}
int foo(int n) {
  print(n);
  pause;
  print(++n);
  pause;
  print(++n);
  pause;
  return n;
}
~~~

We call a function that uses `pause` a coroutine. We would then have a way to
run a bunch of coroutines concurrently and collect their return values:

~~~{.dart}
main() {
  var results = run([foo(0), foo(10)]);
  print("results = $results");
}
~~~

This program would print something like:

~~~
0
10
1
11
2
22
results = [2, 22]
~~~

The order in which 0,1,2 and 10,11,12 are interleaved depends on the scheduling
strategy implemented by run. Here we assume round-robin.

## Encoding

The idea of the encoding is to represent the suspended computation by its
continuation (a closure). So a coroutine is a function that either returns
a continuation or a value when it's done. Something like
`Computation = Paused(continuation) | Done(value)` if we had algebraic datatypes
in Dart. We don't so we encode it as two classes.

~~~{.dart}
class Computation {}

class Paused implements Computation {
  final Function continuation;
  Paused(this.continuation);
}

class Done implements Computation {
  final value;
  Done(this.value);
}
~~~

Now we can rewrite `foo` to return suspended computations whenever there was
a `pause` in our original version.

~~~{.dart}
Computation foo(int n) {
  print(n);
  return new Paused(() {
    print(++n);
    return new Paused(() {
      print(++n);
      return new Paused(() {
        return new Done(n);
      });
    });
  });
}
~~~

We can now define `run`, which round-robins through the computations and calls
the paused ones until everyone is done:

~~~{.dart}
List run(List<Computation> computations) {
  while (!computations.every((c) => c is Done)) {
    for (int i = 0; i < computations.length; i++) {
      var c = computations[i];
      if (c is Paused) {
        computations[i] = c.continuation();
      }
    }
  }
  return computations
      .map((c) => c.value)
      .toList();
}
~~~

And `main` behaves exactly as expected:

~~~
0
10
1
11
2
22
results = [2, 22]
~~~

## Composability

This encoding does not compose well. In our imaginary language with `pause` we
expect to be able to write:

~~~{.dart}
int twoTimesFoo(int n) {
  var m = foo(n);
  return foo(m);
}
~~~

Running `foo(0)` would then print

~~~
0
1
2
2
3
4
~~~

and return `4`. Encoding `twoTimesFoo` using `Computation`s isn't
straightforward though. We could try something like this:

~~~{.dart}
Computation twoTimesFoo(int n) {
  var mlist = run([foo(n)]);
  return foo(mlist.single);
}

main() {
  print(run([twoTimesFoo(0)]))
}
~~~

It seems to be working:

~~~
0
1
2
2
3
4
[4]
~~~

But the call to `run` inside `twoTimesFoo` is "atomic" from the point of view
of the outer `run`, the one inside `main`. We can see this is the case by
running two `twoTimesFoo`s concurrently:

~~~{.dart}
main() {
  print(run([twoTimesFoo(0), twoTimesFoo(10)]))
}
~~~

Which prints:

~~~
0
1
2
2
10
11
12
12
3
13
4
14
[4, 14]
~~~

This is not what we want. We want `0, 10, 1, 11, 2, 12, ...`. So the actual
correct way of encoding `twoTimesFoo` is:

~~~{.dart}
Computation twoTimesFoo(int n) {
  return twoTimesFooAux(foo(n));
}

Computation twoTimesFooAux(Computation c) {
  return (c is Paused)
      ? new Paused(() => twoTimesFooAux(c.continuation()))
      : foo(c.value);
}
~~~

That is, we call `foo(n)`, and as long as it is paused we yield back control
to the main loop with a continuation that resumes `c`. This time we get the
expected output:

~~~
0
10
1
11
2
12
2
12
3
13
4
14
[4, 14]
~~~

This code is really hard to parse though. And this trick needs to be replicated
for *every* call to a sub-coroutine. It would be nice if we could hide this
plumbing behind some function, if we could somehow chain computations.

We introduce the method `then` on `Computation`s which does exactly that. It
takes computation and a function that expects a value and returns a computation.
We use it like this:

~~~{.dart}
Computation twoTimesFoo(int n) {
  return foo(n).then((m) {
    return foo(m);
  });
}
~~~

Informally, the method `then` works like this:

~~~
Done(x)   then f = f(x)
Paused(k) then f = Paused(() => k() then f)
~~~

That is, chaining a done computation with a function is like feeding the
function with the result of the computation. Chaining a paused computation
with a function is like having a computation whose result will eventually
be fed to that function.

Here is how `then` is defined in Dart:

~~~{.dart}
class Computation {
  Computation then(Function f);
}

class Paused implements Computation {
  final Function continuation;
  Paused(this.continuation);

  Computation then(Function f) {
    return new Paused(() => continuation().then(f));
  }
}

class Done implements Computation {
  final value;
  Done(this.value);

  Computation then(Function f) {
    return f(value);
  }
}
~~~

Turns out `Computation`, `Done` and `then` form together what is called a monad.
Not that it matters much.

And that's it! We can now happily pretend we have `pause` in Dart.
