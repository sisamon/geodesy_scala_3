package com.a3.capex.geodesy

import munit.FunSuite
import com.a3.capex.geodesy.ShapesCore.Point
import com.a3.capex.geodesy.Coordinates.{Latitude, Longitude}
import org.tinfour.common.Vertex
import squants.space.AngleConversions._
import com.a3.capex.geodesy.TinfourConverters.given_Conversion_Point_Vertex
import com.a3.capex.geodesy.TinfourConverters.given_Conversion_Vertex_Point
import neotype.unwrap // For accessing underlying values of Newtypes

class TinfourConvertersSuite extends FunSuite {

  test("Point to Vertex conversion") {
    val point = Point(Latitude.unsafeMake(10.degrees), Longitude.unsafeMake(20.degrees))
    val vertex: Vertex = point // Implicit conversion

    assertEquals(vertex.getX, 20.0, "Vertex X should match Point longitude")
    assertEquals(vertex.getY, 10.0, "Vertex Y should match Point latitude")
    assertEquals(vertex.getZ, 0.0, "Vertex Z should be 0.0")
  }

  test("Vertex to Point conversion") {
    val vertex = new Vertex(45.0, 30.0, 5.0) // X is longitude, Y is latitude
    val point: Point = vertex // Implicit conversion

    assertEquals(point.latitude.unwrap.toDegrees, 30.0, "Point latitude should match Vertex Y")
    assertEquals(point.longitude.unwrap.toDegrees, 45.0, "Point longitude should match Vertex X")
    // Note: Z value from Vertex is ignored in the conversion to Point as Point is 2D.
  }

  test("Round-trip conversion: Point -> Vertex -> Point") {
    val originalPoint = Point(Latitude.unsafeMake((-33.3).degrees), Longitude.unsafeMake(123.45.degrees))
    
    val vertexFromPoint: Vertex = originalPoint
    val finalPointFromVertex: Point = vertexFromPoint

    val tolerance = 1e-9 // Standard tolerance for floating-point comparisons
    assertEqualsDouble(finalPointFromVertex.latitude.unwrap.toDegrees, originalPoint.latitude.unwrap.toDegrees, tolerance, "Latitude should match after round trip")
    assertEqualsDouble(finalPointFromVertex.longitude.unwrap.toDegrees, originalPoint.longitude.unwrap.toDegrees, tolerance, "Longitude should match after round trip")
  }
}
