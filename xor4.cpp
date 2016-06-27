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

#include <iostream>
#include <string>
#include <gmpxx.h>

class Expr {
 public:
  virtual std::string pretty() = 0;
  virtual bool eval(bool a, bool b, bool c, bool d) = 0;
  virtual ~Expr() {}
};

class A : public Expr {
 public:
  std::string pretty() {
    return "a";
  }

  bool eval(bool a, bool b, bool c, bool d) {
    return a;
  }
};

class B : public Expr {
 public:
  std::string pretty() {
    return "b";
  }

  bool eval(bool a, bool b, bool c, bool d) {
    return b;
  }
};

class C : public Expr {
 public:
  std::string pretty() {
    return "c";
  }

  bool eval(bool a, bool b, bool c, bool d) {
    return c;
  }
};

class D : public Expr {
 public:
  std::string pretty() {
    return "d";
  }

  bool eval(bool a, bool b, bool c, bool d) {
    return d;
  }
};

class Not : public Expr {
 public:
  Expr* const e;

  Not(Expr* const e) : e(e) {};

  ~Not() {
    delete e;
  }

  std::string pretty() {
    return "not(" + e->pretty() + ")";
  }

  bool eval(bool a, bool b, bool c, bool d) {
    return !e->eval(a, b, c, d);
  }
};

class And : public Expr {
 public:
  Expr* const e1;
  Expr* const e2;

  And(Expr* const e1, Expr* const e2) : e1(e1), e2(e2) {};

  ~And() {
    delete e1;
    delete e2;
  }

  std::string pretty() {
    return "and(" + e1->pretty() + ", " + e2->pretty() + ")";
  }

  bool eval(bool a, bool b, bool c, bool d) {
    return e1->eval(a, b, c, d) && e2->eval(a, b, c, d);
  }
};

class Or : public Expr {
 public:
  Expr* const e1;
  Expr* const e2;

  Or(Expr* const e1, Expr* const e2) : e1(e1), e2(e2) {};

  ~Or() {
    delete e1;
    delete e2;
  }

  std::string pretty() {
    return "or(" + e1->pretty() + ", " + e2->pretty() + ")";
  }

  bool eval(bool a, bool b, bool c, bool d) {
    return e1->eval(a, b, c, d) || e2->eval(a, b, c, d);
  }
};

class Xor : public Expr {
 public:
  Expr* const e1;
  Expr* const e2;

  Xor(Expr* const e1, Expr* const e2) : e1(e1), e2(e2) {};

  ~Xor() {
    delete e1;
    delete e2;
  }

  std::string pretty() {
    return "xor(" + e1->pretty() + ", " + e2->pretty() + ")";
  }

  bool eval(bool a, bool b, bool c, bool d) {
    return e1->eval(a, b, c, d) ^ e2->eval(a, b, c, d);
  }
};

Expr* const ba() { return new A(); }
Expr* const bb() { return new B(); }
Expr* const bc() { return new C(); }
Expr* const bd() { return new D(); }
Expr* bnot(Expr* const e) { return new Not(e); }
Expr* band(Expr* const e1, Expr* const e2) { return new And(e1, e2); }
Expr* bor(Expr* const e1, Expr* const e2) { return new Or(e1, e2); }
Expr* bxor(Expr* const e1, Expr* const e2) { return new Xor(e1, e2); }

Expr* const knownXor4 =
  band(
      bor(bxor(ba(), bb()), bxor(bc(), bd())),
      bxor(bor(ba(), bb()), bor(bc(), bd())));

bool isXor4(Expr* const e) {
  return !e->eval(true, true, true, true)
      && !e->eval(false, true, true, true)
      && !e->eval(true, false, true, true)
      && !e->eval(false, false, true, true)
      && !e->eval(true, true, false, true)
      && !e->eval(false, true, false, true)
      && !e->eval(true, false, false, true)
      && e->eval(false, false, false, true)
      && !e->eval(true, true, true, false)
      && !e->eval(false, true, true, false)
      && !e->eval(true, false, true, false)
      && e->eval(false, false, true, false)
      && !e->eval(true, true, false, false)
      && e->eval(false, true, false, false)
      && e->eval(true, false, false, false)
      && !e->eval(false, false, false, false);
}

class Finite {
 public:
  mpz_class cardinal;

  Finite(mpz_class cardinal) : cardinal(cardinal) {};
  virtual Expr* const get(mpz_class index) = 0;
};

class Union : public Finite {
 public:
  Finite* const f1;
  Finite* const f2;

  Union(Finite * const f1, Finite* const f2)
      : Finite(f1->cardinal + f2->cardinal)
      , f1(f1)
      , f2(f2) {}

  Expr* const get(mpz_class index) {
    return (index < f1->cardinal)
        ? f1->get(index)
        : f2->get(index - f1->cardinal);
  }
};

class Singleton : public Finite {
 public:
  Expr* const (*constructor)();

  Singleton(Expr* const (*constructor)())
      : Finite(1)
      , constructor(constructor) {};

  Expr* const get(mpz_class index) {
    if (index == 0) {
      return constructor();
    }
    throw std::out_of_range("");
  }
};

class Unary : public Finite {
 public:
  Expr* (*constructor)(Expr* const);
  Finite* const f;

  Unary(Expr* (*constructor)(Expr* const), Finite* const f)
      : Finite(f->cardinal)
      , constructor(constructor)
      , f(f) {}

  Expr* const get(mpz_class index) {
    return constructor(f->get(index));
  }
};

class Binary : public Finite {
 public:
  Expr* (*constructor)(Expr* const, Expr* const);
  Finite* const f1;
  Finite* const f2;

  Binary(Expr* (*constructor)(Expr* const, Expr* const),
         Finite* const f1,
         Finite* const f2)
      : Finite(f1->cardinal * f2->cardinal)
      , constructor(constructor)
      , f1(f1)
      , f2(f2) {}

  Expr* const get(mpz_class index) {
    return constructor(
        f1->get(index / f2->cardinal),
        f2->get(index % f2->cardinal));
  }
};

Finite* exprsOfSize(int size) {
  Finite* parts[size];
  parts[0] = new Union(new Singleton(ba),
                       new Union(new Singleton(bb),
                                 new Union(new Singleton(bc),
                                           new Singleton(bd))));
  for (int i = 1; i < size; i++) {
    Finite* f = new Unary(&bnot, parts[i-1]);
    for (int j = 0; j < i-1; j++) {
      f = new Union(f, new Binary(&band, parts[j], parts[i-2-j]));
      f = new Union(f, new Binary(&bor, parts[j], parts[i-2-j]));
      f = new Union(f, new Binary(&bxor, parts[j], parts[i-2-j]));
    }
    parts[i] = f;
  }
  return parts[size-1];
}

int main(int argc, char** argv) {
  std::cout << "isXor4 " << knownXor4->pretty() << ": " << isXor4(knownXor4) << std::endl;
  Finite* const f = exprsOfSize(15);
  std::cout << f->cardinal << std::endl;
  for (mpz_class i = 0; i < f->cardinal; i++) {
    Expr* e = f->get(i);
    if (isXor4(e)) {
      std::cout << e->pretty() << std::endl;
    }
    delete e;
  }
}
