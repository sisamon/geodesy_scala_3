package com.a3.capex.geodesy

import org.tinfour.common.Vertex
import com.a3.capex.geodesy.ShapesCore.Point // Point is defined within the ShapesCore object
import com.a3.capex.geodesy.Coordinates.{Latitude, Longitude}
import squants.space.AngleConversions._
import neotype.unwrap

object TinfourConverters {
  given Conversion[Point, Vertex] = (p: Point) => 
    new Vertex(p.longitude.unwrap.toDegrees, p.latitude.unwrap.toDegrees, 0.0)

  given Conversion[Vertex, Point] = (v: Vertex) => 
    Point(
      Latitude.unsafeMake(v.getY.degrees),
      Longitude.unsafeMake(v.getX.degrees)
    )
}
