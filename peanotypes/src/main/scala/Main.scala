object Main extends App {

  trait Nat {
    type +[A <: Nat] <: Nat
    type *[A <: Nat] <: Nat
    type ! <: Nat
  }

  trait Z extends Nat {
    type +[A <: Nat] = A
    type *[A <: Nat] = Z
    type ! = _1
  }

  trait S[N <: Nat] extends Nat {
    type +[A <: Nat] = N# +[S[A]]
    type *[A <: Nat] = A# +[N# *[A]]
    type ! = S[N]# *[N# !]
  }

  type _1 = S[Z]
  type _2 = S[_1]
  type _3 = S[_2]
  type _4 = S[_3]
  type _5 = S[_4]
  type _6 = S[_5]

  implicitly[_3# ! =:= _6]
}
