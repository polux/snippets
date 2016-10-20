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

import java.util.function.Function;

public class Patterns {

  static class Pair<S,T> {
    final S fst;
    final T snd;

    Pair(S fst, T snd) {
      this.fst = fst;
      this.snd = snd;
    }
  }

  static <T> Function<T,T> nil() {
    return k -> k;
  }

  static <S,T> Function<Function<S, T>,T> one(S v) {
    return k -> k.apply(v);
  }

  static <S,T,U> Function<S,U> append(Function<S,T> m, Function<T,U> n) {
    return k -> n.apply(m.apply(k));
  }

  static <S,T> Function<S, Function<Function<S,T>,T>> var() {
    return v -> one(v);
  }

  static <S,T> Function<S, Function<T, T>> cst(S c) {
    return v -> {
      if (v.equals(c)) {
        return nil();
      } else {
        throw new RuntimeException("match error");
      }
    };
  }

  static <A,B,S,T,U> Function<Pair<A,B>, Function<S,U>> pair(Function<A, Function<S,T>> p, Function<B, Function<T,U>> q) {
    return v -> append(p.apply(v.fst), q.apply(v.snd));
  }

  static <U> Function<Pair<String, Pair<Integer, String>>, Function<Function<String, Function<String, U>>, U>> pattern() {
    return pair(var(), pair(cst(1), var()));
  }

  public static void main(String[] args) {
    Pair<String, Pair<Integer, String>> subject = new Pair<>("a", new Pair<>(1, "b"));
    String result = Patterns.<String>pattern().apply(subject).apply(x -> y -> x + y);
    System.out.println(result);
  }
}
