package com.a3.capex.geodesy

import com.a3.capex.geodesy.Coordinates.{Longitude, Latitude}
import com.a3.capex.geodesy.CoordinatesImplicits.{given_Conversion_Longitude_Angle, given_Conversion_Latitude_Angle}
import com.a3.capex.geodesy.ShapesCore.{CurvedShapes, Box, Point}

import squants.space.Angle
import java.lang.Math._


object Edges:

  case class Edge(origin: Point, end: Point) extends CurvedShapes:
    override def barycenter: Point = Point(Latitude.unsafeMake((origin.latitude + end.latitude) / 2), Longitude.unsafeMake((origin.longitude + end.longitude) / 2))

    override def north: Latitude = origin.latitude

    override def south: Latitude = end.latitude

    override def east: Longitude = origin.longitude

    override def west: Longitude = end.longitude

    // An edge can not contain anything.
    override def contains(geom: Box): Boolean = false

    override def toString: String = s"($origin.latitude , $origin.longitude  -> $end.latitude , $end.longitude )"

    override def isDegenerate: Boolean = origin == end
    
    // Sorted by latitude, used to ensure membership of edges
    def sorted(): Edge =
      if (origin.isSorted(end)) Edge(origin, end) else Edge(end, origin)

    def doIntersect(edge2: Edge): Boolean =
      val p1 = this.origin
      val q1 = this.end
      val p2 = edge2.origin
      val q2 = edge2.end

      val o1: Int = Point.orientation(p1, q1, p2)
      val o2: Int = Point.orientation(p1, q1, q2)
      val o3: Int = Point.orientation(p2, q2, p1)
      val o4: Int = Point.orientation(p2, q2, q1)


      if (o1 != o2 && o3 != o4) return true // General case
      if (o1 == 0 && Point.onSegment(p1, p2, q1)) return true // p1, q1, and p2 are collinear and p2 lies on segment p1q1
      if (o2 == 0 && Point.onSegment(p1, q2, q1)) return true // p1, q1, and q2 are collinear and q2 lies on segment p1q1
      if (o3 == 0 && Point.onSegment(p2, p1, q2)) return true // p2, q2, and p1 are collinear and p1 lies on segment p2q2
      if (o4 == 0 && Point.onSegment(p2, q1, q2)) return true // p2, q2, and q1 are collinear and q1 lies on segment p2q2
      false // If none of the cases


  object Edge:
    // Equality instance for Edge using derived equality
    given CanEqual[Edge, Edge] = CanEqual.derived
    given Equiv[Edge] = Equiv.fromFunction(_ == _)
    
    given Conversion[Edge, Box] = _.toBox

