//  __..                                                                                   Copyright (c) 2013, Alois Cochard
// (__ |_  _ ._  _.
// .__)[ )(_)[ )(_]
//
// Licensed under the Apache License, Version 2.0

package shona

package record {
  import shapeless._
  import TypeOperators._

  trait Record[N <: String, T] { def label: Label[N] }

  trait RecordFactory[R[_ <: String, _] <: Record[_, _]] {
    class RecordBuilder[N <: String](label: Label[N]) { def ~[T](value: T) = create(label, value) }
    final def apply[N <: String]()(implicit label: Label[N]) = new RecordBuilder(label)
    def create[N <: String, T](label: Label[N], value: T): R[N, T]
  }

  /** Record transformation **/
  trait RecordT[F[_ <: String, _], G[_ <: String, _]] extends Poly1 {
    def apply[N <: String, T](f : F[N, T]) : G[N, T]
    implicit def caseUniv[N <: String, T] = at[F[N, T]](apply(_))
  }

  object RecordT {
    implicit def inst1[F[_ <: String, _], G[_ <: String, _], N <: String, T](f: RecordT[F, G]): F[N, T] => G[N, T] = f(_)
  }

  /** Typeclass witnessing that `L1` and `L2` have records of the form `F1[N, T]` and `F2[N, T]` **/
  trait RecordTRel[L1 <: HList, F1[_ <: String, _], L2 <: HList, F2[_ <: String, _]] {
    def map(nt: RecordT[F1, F2], fa: L1): L2
  }

  object RecordTRel {
    implicit def hnilRecordTRel1[F1[_ <: String, _], F2[_ <: String, _]] = new RecordTRel[HNil, F1, HNil, F2] {
      def map(f: RecordT[F1, F2], fa: HNil): HNil = HNil
    }

    implicit def hlistRecordTRel1[N <: String, T, F1[_ <: String, _], F2[_ <: String, _], T1 <: HList, T2 <: HList](implicit rt : RecordTRel[T1, F1, T2, F2]) =
      new RecordTRel[F1[N, T] :: T1, F1, F2[N, T] :: T2, F2] {
        def map(f: RecordT[F1, F2], fa: F1[N, T] :: T1): F2[N, T] :: T2 = f(fa.head) :: rt.map(f, fa.tail)
      }
  }
}

