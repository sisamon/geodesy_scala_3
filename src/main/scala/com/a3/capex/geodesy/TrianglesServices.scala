package com.a3.capex.geodesy

//import Coordinates._
//import CoordinatesImplicits.{
// //  given_Conversion_Longitude_, given_Conversion_Latitude_Double,
// given_Conversion_Longitude_Angle, given_Conversion_Latitude_Angle,
// given_Conversion_Angle_Longitude, given_Conversion_Angle_Latitude,
// given_Conversion_Angle_Double
// // ,LatitudeOrder, LongitudeOrder
//}
//import ShapesCore.PointImplicits._
import com.a3.capex.geodesy.ShapesCore.*
import com.a3.capex.geodesy.Triangles.*
//import com.a3.capex.geodesy.TinfourConverters._
import com.a3.capex.geodesy.TinfourConverters.given_Conversion_Point_Vertex
import com.a3.capex.geodesy.TinfourConverters.given_Conversion_Vertex_Point

//import squants._

import scala.jdk.CollectionConverters.*
import org.tinfour.standard.IncrementalTin
//import org.tinfour.common.Vertex
//import com.a3.capex.geodesy.Coordinates.{Latitude, Longitude} // Assuming Latitude/Longitude are in Coordinates object
//import squants.space.AngleConversions._ // For .degrees
//import org.tinfour.common.Vertex // Make sure Vertex is imported
//

object TrianglesServices:
  def Delaunay(vertexes: List[Point]): List[Triangle] =
    val tin = new IncrementalTin()
    vertexes.foreach(p => tin.add(p))
    if !tin.isBootstrapped then throw new RuntimeException("TIN failed to initialize (not enough points?)")
    tin.triangles.asScala.map { st =>
      val p1: Point = st.getVertexA // Implicitly converts Vertex to Point
      val p2: Point = st.getVertexB // Implicitly converts Vertex to Point
      val p3: Point = st.getVertexC // Implicitly converts Vertex to Point
      Triangle(p1, p2, p3)
    }.toList
