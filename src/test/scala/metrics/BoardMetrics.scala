import com.nyavro.BoardParser
import org.scalameter.api._
import org.scalameter.picklers.noPickler._

object BoardMetrics extends Bench.LocalTime with BoardParser {
  override def persistor = new SerializationPersistor
  val pieces = Gen.enumeration("pieces")('-'::piecesMap.keys.toList:_*)
  val sizes = Gen.range("sizes")(1, 20, 1)
}