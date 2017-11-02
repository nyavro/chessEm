import org.scalameter.api._
import org.scalameter.picklers.noPickler._

for {
  v <- 1 to 10
  u <- 2 to 5
} yield {
  (u)
}

