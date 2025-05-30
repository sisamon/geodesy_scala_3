import squants.space.Angle
import squants.space.AngleConversions.*
import com.a3.capex.geodesy.Coordinates.Longitude
import com.a3.capex.geodesy.CoordinatesImplicits.{
  given_Conversion_Angle_Longitude,
  given_Conversion_Double_Angle
}  // given_Conversion_Angle_Double,
import org.tinfour.standard.IncrementalTin
import org.tinfour.common.Vertex
import scala.jdk.CollectionConverters._


@main
def main(): Unit = 

  println("I am not crazy. Hello world!")
  val d1: Double = 45.0
  val d2: Double = 18.5
  val a1: Angle = d1
  val l1: Longitude = a1 // Longitude.unsafeMake(d1.degrees)
  val l2: Longitude = Longitude.unsafeMake(d2.degrees)

  val l: Longitude = l1 + l2
  // It would require given_Conversion_Angle_Double
  // but then I get into an infinite loop with given_Conversion_Double_Angle
  // Luckily this is not a use case we will face often, I think.
  // val d: Double = a1

  println(l)
  val tin = new IncrementalTin()

  val vertices = Seq(
    new Vertex(0, 0, 0),
    new Vertex(1, 0, 0),
    new Vertex(0, 1, 0),
    new Vertex(1, 1, 0)
  )

  vertices.foreach(tin.add)
  if (!tin.isBootstrapped)
    throw new RuntimeException("TIN failed to initialize (not enough points?)")
  println("Delaunay Triangles:")
  val triangles = tin.triangles.asScala
  triangles.foreach ( tri =>
      val a = tri.getVertexA
      val b = tri.getVertexB
      val c = tri.getVertexC
      val coords = Seq(a, b, c)
        .map(v => f"(${v.getX}%.2f, ${v.getY}%.2f)")
        .mkString(" - ")
      println(s"Triangle: $coords")
  )




//
//object TinfourExample extends App {
//  val tin = new IncrementalTin()
//
//  val vertices = Seq(
//    new Vertex(0, 0, 0),
//    new Vertex(1, 0, 0),
//    new Vertex(0, 1, 0),
//    new Vertex(1, 1, 0)
//  )
//
//  tin.add(vertices.asJavaCollection)
//
//  val triangles = tin.getTriangles.asScala
//  triangles.foreach { tri =>
//    val verts = tri.getVertices
//    println("Triangle: " + verts.map(v => f"(${v.getX}%.2f, ${v.getY}%.2f)").mkString(" "))
//  }
//}
