package com.a3.capex.geodesy

import Coordinates.{Latitude, Longitude, R}
import ShapesCore._
import TypeclassInstances.{given, _}
import squants.space.{Angle, Degrees, Length, SquareMeters, Area}
import squants.space.AngleConversions._
import squants.space.LengthConversions._
import java.lang.Math._
import scala.annotation.tailrec
import neotype._
import scala.language.implicitConversions
import squants.space.AreaConversions.AreaNumeric

// Type aliases for better readability
type Lat = Latitude
type Lon = Longitude


object Triangles:
  //      val px1 = normalizeLongitude(longitude1).min(normalizeLongitude(geom.longitude1))
  //      val py1 = latitude1.unwrap.min(geom.latitude1.unwrap)
  //      val px2 = normalizeLongitude(longitude2).max(normalizeLongitude(geom.longitude2))
  //      val py2 = latitude2.unwrap.max(geom.latitude2.unwrap)
  //      val a = (py2.toDegrees - py1.toDegrees) * (px2.toDegrees - px1.toDegrees) * SquareMeters(1)
  //      a - area
  //


  case class Triangle(p1: Point, p2: Point, p3: Point) extends CurvedShapes:
    /**
     * Calculates the barycenter (centroid) of the triangle.
     * For geographic coordinates, this is a simple average of the vertices.
     * Note: For large triangles, this is an approximation that doesn't account for
     * the spherical nature of the Earth. For more accurate results, consider
     * converting to 3D coordinates and calculating the centroid there.
     */
    override def barycenter: Point = 
      // Calculate average latitude and longitude
      val avgLat = p1.latitude.average(p2.latitude).average(p3.latitude)
      val avgLon = p1.longitude.average(p2.longitude).average(p3.longitude)
      Point(avgLat, avgLon)

    /** Returns the easternmost longitude of the triangle */
    override def east: Lon =
      List(p1.longitude, p2.longitude, p3.longitude).max
    
    /** Returns the northernmost latitude of the triangle */
    override def north: Lat =
      List(p1.latitude, p2.latitude, p3.latitude).max
    
    /** Returns the southernmost latitude of the triangle */
    override def south: Lat =
      List(p1.latitude, p2.latitude, p3.latitude).min
    
    /** Returns the westernmost longitude of the triangle */
    override def west: Lon =
      List(p1.longitude, p2.longitude, p3.longitude).min

    override def toString: String = s"[$p1 , $p2 , $p3]"  //p1.toString + " , " + p2.toString + " , " + p3.toString

    // Some basic geometry that I will need to revisit.
    override def area: Area =
      if p1 == p2 || p1 == p3 || p2 == p3 then
        val lat1 = p1.latitude.unwrap.toDegrees
        val lon1 = p1.longitude.unwrap.toDegrees
        val lat2 = p2.latitude.unwrap.toDegrees
        val lon2 = p2.longitude.unwrap.toDegrees
        val lat3 = p3.latitude.unwrap.toDegrees
        val lon3 = p3.longitude.unwrap.toDegrees
        
        // Using the shoelace formula for area calculation
        val areaValue = R * R * abs(
          lat1 * lon2 + 
          lat2 * lon3 + 
          lat3 * lon1 - 
          lat1 * lon3 - 
          lat2 * lon1 - 
          lat3 * lon2
        )
        SquareMeters(areaValue)
      else
        SquareMeters(0)

    // Distance will be used to link to specific stations
    // We currently use the distance to the barycenter
    override def distance(pt: Point): Length = barycenter.distance(pt)

//    override def distanceSquared(pt: Point): Double = 
//      val dist = barycenter.distance(pt)
//      dist * dist

    def sorted: Triangle =
      if p1.isSorted(p2) && p1.isSorted(p3) && p2.isSorted(p3) then Triangle(p1, p2, p3)
      else if p1.isSorted(p2) && p1.isSorted(p3) && p3.isSorted(p2) then Triangle(p1, p3, p2)
      else if p2.isSorted(p1) && p2.isSorted(p3) && p1.isSorted(p3) then Triangle(p2, p1, p3)
      else if p2.isSorted(p1) && p2.isSorted(p3) && p3.isSorted(p1) then Triangle(p2, p3, p1)
      else if p3.isSorted(p1) && p3.isSorted(p2) && p1.isSorted(p2) then Triangle(p3, p2, p1)
      else Triangle(p3, p1, p2) // (p3.isSorted(p1) && p3.isSorted(p2) && p2.isSorted(p1))

    def isDegenerate: Boolean = p1 == p2 || p1 == p3 || p2 == p3

    def partition(level: Int, puntosTriangulacion: Array[Point.PointKey]): List[Triangle] =
      def midTriangles(t: Triangle): List[Triangle] =
        List(
          Triangle(t.p1, t.p1.middle(t.p2), t.p1.middle(t.p3)),
          Triangle(t.p2, t.p2.middle(t.p1), t.p2.middle(t.p3)),
          Triangle(t.p3, t.p3.middle(t.p2), t.p3.middle(t.p1)),
          Triangle(t.p1.middle(t.p2), t.p2.middle(t.p3), t.p3.middle(t.p1))
        )

      def maxsize(t: Triangle): Boolean =
        val containsPoint = puntosTriangulacion.contains(t.p1.keyCoordinates) ||
                          puntosTriangulacion.contains(t.p2.keyCoordinates) ||
                          puntosTriangulacion.contains(t.p3.keyCoordinates)
        
        // Calculate distances between all points
        val d1 = t.p1.distance(t.p2).toKilometers
        val d2 = t.p1.distance(t.p3).toKilometers
        val d3 = t.p2.distance(t.p3).toKilometers
        
        // Find the maximum distance
        val maxDistance = d1.max(d2).max(d3)
        
        containsPoint && (maxDistance > 10.0)

      @tailrec
      def generateChildren(level: Int, triangles: List[Triangle], children: List[Triangle]): List[Triangle] =
        (level, triangles) match
          case (_, Nil) => children
          case (1, xs) => xs ::: children
          case (n, xs) =>
            val newTriangles = xs.flatMap(t => if maxsize(t) then midTriangles(t) else List(t))
            generateChildren(n - 1, newTriangles, children)

      if level >= 1 then generateChildren(level, midTriangles(this), Nil)
      else Nil


  object Triangle:
    /**
     * Equality instance for Triangle that uses structural equality.
     * Two triangles are considered equal if they have the same three points,
     * regardless of the order of the points.
     */
    given Equiv[Triangle] = Equiv.fromFunction { (t1, t2) =>
      val t1Points = Set(t1.p1, t1.p2, t1.p3)
      val t2Points = Set(t2.p1, t2.p2, t2.p3)
      t1Points == t2Points
    }
    
    given CanEqual[Triangle, Triangle] = CanEqual.derived