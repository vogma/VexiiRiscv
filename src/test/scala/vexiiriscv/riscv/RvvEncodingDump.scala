package vexiiriscv.riscv

/*
 * Prints every vector encoding of Rvv as "name match mask" (hex), one per line, for an external cross-check.
 *
 *   sbt "Test/runMain vexiiriscv.riscv.RvvEncodingDump"
 */
object RvvEncodingDump extends App {
  for ((name, d) <- Rvv.all; k <- d.keys) {
    println(f"$name ${k.value.toLong}%08x ${k.careAbout.toLong}%08x")
  }
}
