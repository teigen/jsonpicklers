package jsonpicklers

trait FlattenTilde {
  type ~[+A, +B] = (A, B)
  val ~ = Tuple2

  private class Tilde[A](a:A){
    def ~[B](b:B) = (a, b)
  }
  private implicit def tilde[A](a:A) = new Tilde[A](a)

  implicit def flatten2[A1,A2,R](f:(A1,A2) => R) = (p: A1~A2) => p match { case (a1~a2) => f(a1,a2) }
  implicit def tilde2[A1,A2](f:((A1,A2))) = f match { case (a1,a2) => a1~a2}
  implicit def flatten3[A1,A2,A3,R](f:(A1,A2,A3) => R) = (p: A1~A2~A3) => p match { case (a1~a2~a3) => f(a1,a2,a3) }
  implicit def tilde3[A1,A2,A3](f:((A1,A2,A3))) = f match { case (a1,a2,a3) => a1~a2~a3}
  implicit def flatten4[A1,A2,A3,A4,R](f:(A1,A2,A3,A4) => R) = (p: A1~A2~A3~A4) => p match { case (a1~a2~a3~a4) => f(a1,a2,a3,a4) }
  implicit def tilde4[A1,A2,A3,A4](f:((A1,A2,A3,A4))) = f match { case (a1,a2,a3,a4) => a1~a2~a3~a4}
  implicit def flatten5[A1,A2,A3,A4,A5,R](f:(A1,A2,A3,A4,A5) => R) = (p: A1~A2~A3~A4~A5) => p match { case (a1~a2~a3~a4~a5) => f(a1,a2,a3,a4,a5) }
  implicit def tilde5[A1,A2,A3,A4,A5](f:((A1,A2,A3,A4,A5))) = f match { case (a1,a2,a3,a4,a5) => a1~a2~a3~a4~a5}
  implicit def flatten6[A1,A2,A3,A4,A5,A6,R](f:(A1,A2,A3,A4,A5,A6) => R) = (p: A1~A2~A3~A4~A5~A6) => p match { case (a1~a2~a3~a4~a5~a6) => f(a1,a2,a3,a4,a5,a6) }
  implicit def tilde6[A1,A2,A3,A4,A5,A6](f:((A1,A2,A3,A4,A5,A6))) = f match { case (a1,a2,a3,a4,a5,a6) => a1~a2~a3~a4~a5~a6}
  implicit def flatten7[A1,A2,A3,A4,A5,A6,A7,R](f:(A1,A2,A3,A4,A5,A6,A7) => R) = (p: A1~A2~A3~A4~A5~A6~A7) => p match { case (a1~a2~a3~a4~a5~a6~a7) => f(a1,a2,a3,a4,a5,a6,a7) }
  implicit def tilde7[A1,A2,A3,A4,A5,A6,A7](f:((A1,A2,A3,A4,A5,A6,A7))) = f match { case (a1,a2,a3,a4,a5,a6,a7) => a1~a2~a3~a4~a5~a6~a7}
  implicit def flatten8[A1,A2,A3,A4,A5,A6,A7,A8,R](f:(A1,A2,A3,A4,A5,A6,A7,A8) => R) = (p: A1~A2~A3~A4~A5~A6~A7~A8) => p match { case (a1~a2~a3~a4~a5~a6~a7~a8) => f(a1,a2,a3,a4,a5,a6,a7,a8) }
  implicit def tilde8[A1,A2,A3,A4,A5,A6,A7,A8](f:((A1,A2,A3,A4,A5,A6,A7,A8))) = f match { case (a1,a2,a3,a4,a5,a6,a7,a8) => a1~a2~a3~a4~a5~a6~a7~a8}
  implicit def flatten9[A1,A2,A3,A4,A5,A6,A7,A8,A9,R](f:(A1,A2,A3,A4,A5,A6,A7,A8,A9) => R) = (p: A1~A2~A3~A4~A5~A6~A7~A8~A9) => p match { case (a1~a2~a3~a4~a5~a6~a7~a8~a9) => f(a1,a2,a3,a4,a5,a6,a7,a8,a9) }
  implicit def tilde9[A1,A2,A3,A4,A5,A6,A7,A8,A9](f:((A1,A2,A3,A4,A5,A6,A7,A8,A9))) = f match { case (a1,a2,a3,a4,a5,a6,a7,a8,a9) => a1~a2~a3~a4~a5~a6~a7~a8~a9}
  implicit def flatten10[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,R](f:(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10) => R) = (p: A1~A2~A3~A4~A5~A6~A7~A8~A9~A10) => p match { case (a1~a2~a3~a4~a5~a6~a7~a8~a9~a10) => f(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) }
  implicit def tilde10[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10](f:((A1,A2,A3,A4,A5,A6,A7,A8,A9,A10))) = f match { case (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) => a1~a2~a3~a4~a5~a6~a7~a8~a9~a10}
  implicit def flatten11[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,R](f:(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11) => R) = (p: A1~A2~A3~A4~A5~A6~A7~A8~A9~A10~A11) => p match { case (a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11) => f(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) }
  implicit def tilde11[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11](f:((A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11))) = f match { case (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) => a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11}
  implicit def flatten12[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,R](f:(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12) => R) = (p: A1~A2~A3~A4~A5~A6~A7~A8~A9~A10~A11~A12) => p match { case (a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12) => f(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) }
  implicit def tilde12[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12](f:((A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12))) = f match { case (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) => a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12}
  implicit def flatten13[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,R](f:(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13) => R) = (p: A1~A2~A3~A4~A5~A6~A7~A8~A9~A10~A11~A12~A13) => p match { case (a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13) => f(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) }
  implicit def tilde13[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13](f:((A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13))) = f match { case (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) => a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13}
  implicit def flatten14[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,R](f:(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14) => R) = (p: A1~A2~A3~A4~A5~A6~A7~A8~A9~A10~A11~A12~A13~A14) => p match { case (a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13~a14) => f(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) }
  implicit def tilde14[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14](f:((A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14))) = f match { case (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) => a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13~a14}
  implicit def flatten15[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,R](f:(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15) => R) = (p: A1~A2~A3~A4~A5~A6~A7~A8~A9~A10~A11~A12~A13~A14~A15) => p match { case (a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13~a14~a15) => f(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) }
  implicit def tilde15[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15](f:((A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15))) = f match { case (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) => a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13~a14~a15}
  implicit def flatten16[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,R](f:(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16) => R) = (p: A1~A2~A3~A4~A5~A6~A7~A8~A9~A10~A11~A12~A13~A14~A15~A16) => p match { case (a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13~a14~a15~a16) => f(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16) }
  implicit def tilde16[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16](f:((A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16))) = f match { case (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16) => a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13~a14~a15~a16}
  implicit def flatten17[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,R](f:(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17) => R) = (p: A1~A2~A3~A4~A5~A6~A7~A8~A9~A10~A11~A12~A13~A14~A15~A16~A17) => p match { case (a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13~a14~a15~a16~a17) => f(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17) }
  implicit def tilde17[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17](f:((A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17))) = f match { case (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17) => a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13~a14~a15~a16~a17}
  implicit def flatten18[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,R](f:(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18) => R) = (p: A1~A2~A3~A4~A5~A6~A7~A8~A9~A10~A11~A12~A13~A14~A15~A16~A17~A18) => p match { case (a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13~a14~a15~a16~a17~a18) => f(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18) }
  implicit def tilde18[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18](f:((A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18))) = f match { case (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18) => a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13~a14~a15~a16~a17~a18}
  implicit def flatten19[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,R](f:(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19) => R) = (p: A1~A2~A3~A4~A5~A6~A7~A8~A9~A10~A11~A12~A13~A14~A15~A16~A17~A18~A19) => p match { case (a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13~a14~a15~a16~a17~a18~a19) => f(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19) }
  implicit def tilde19[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19](f:((A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19))) = f match { case (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19) => a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13~a14~a15~a16~a17~a18~a19}
  implicit def flatten20[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,R](f:(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20) => R) = (p: A1~A2~A3~A4~A5~A6~A7~A8~A9~A10~A11~A12~A13~A14~A15~A16~A17~A18~A19~A20) => p match { case (a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13~a14~a15~a16~a17~a18~a19~a20) => f(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20) }
  implicit def tilde20[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20](f:((A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20))) = f match { case (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20) => a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13~a14~a15~a16~a17~a18~a19~a20}
  implicit def flatten21[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,R](f:(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21) => R) = (p: A1~A2~A3~A4~A5~A6~A7~A8~A9~A10~A11~A12~A13~A14~A15~A16~A17~A18~A19~A20~A21) => p match { case (a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13~a14~a15~a16~a17~a18~a19~a20~a21) => f(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21) }
  implicit def tilde21[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21](f:((A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21))) = f match { case (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21) => a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13~a14~a15~a16~a17~a18~a19~a20~a21}
  implicit def flatten22[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22,R](f:(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22) => R) = (p: A1~A2~A3~A4~A5~A6~A7~A8~A9~A10~A11~A12~A13~A14~A15~A16~A17~A18~A19~A20~A21~A22) => p match { case (a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13~a14~a15~a16~a17~a18~a19~a20~a21~a22) => f(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22) }
  implicit def tilde22[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22](f:((A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22))) = f match { case (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22) => a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13~a14~a15~a16~a17~a18~a19~a20~a21~a22}
}