import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}

val pathsGen = Gen.frequency(
  (3, "C:\\Windows\\DigitalLocker"),
  (2, "C:\\Temp"),
  (3, "Invalid folder"),
  (1, "C:\\Program Files\\Internet Explorer"),
  (4, "C:\\Program Files\\Windows Defender Advanced Threat Protection"),
  (1, "C:\\Program Files\\Windows Photo Viewer")
)

val pathsGenContainer = Gen.containerOf[List,String](pathsGen)

object WindowsSpec extends Properties("Windows") {
  property("validPaths") = forAll(pathsGenContainer) { a: List[String] =>
    println(a)
    a.forall(v => v.contains("C:\\"))
  }
}