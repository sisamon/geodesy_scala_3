package com.a3.capex.geodesy

import Coordinates.{Latitude, Longitude, R}
import ShapesCore.*
import TypeclassInstances.given
import squants.space.{Angle, Area, Length, SquareMeters}
import java.lang.Math.*
import scala.annotation.tailrec
import neotype.unwrap
import squants.space.AngleConversions.*
import scala.language.implicitConversions
import squants.space.AreaConversions.AreaNumeric
//import scala.math.Pi

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
    /** Calculates the barycenter (centroid) of the triangle. For geographic coordinates, this is a simple average of
      * the vertices. Note: For large triangles, this is an approximation that doesn't account for the spherical nature
      * of the Earth. For more accurate results, consider converting to 3D coordinates and calculating the centroid
      * there.
      */
    override def barycenter: Point = {
      val avgLatVal = (p1.latitude.unwrap.toDegrees + p2.latitude.unwrap.toDegrees + p3.latitude.unwrap.toDegrees) / 3.0
      val avgLonValDegrees =
        (p1.longitude.unwrap.toDegrees + p2.longitude.unwrap.toDegrees + p3.longitude.unwrap.toDegrees) / 3.0
//      // Normalize the average longitude to be within [-180, 180) degrees
//      val normalizedAvgLonDegrees = ((avgLonValDegrees + 540) % 360) - 180
      // Adjust if the result of % is negative for negative inputs, ensuring it's truly in [-180, 180)
      // Example: -190 -> ((-190 + 540) % 360) - 180 = (350 % 360) - 180 = 350 - 180 = 170. Correct.
      // Example: -550 -> ((-550 + 540) % 360) - 180 = (-10 % 360) - 180. In Scala, % can be negative.
      // (-10 % 360) is -10. So, -10 - 180 = -190. Incorrect.
      // A more robust normalization for [-180, 180) from any angle 'a': val norm = (a % 360 + 360) % 360; if (norm > 180) norm - 360 else norm
      val finalNormalizedLonDegrees = {
        var tempLon = avgLonValDegrees % 360
        if tempLon <= -180 then tempLon += 360
        else if tempLon > 180 then tempLon -= 360
        tempLon
      }
      Point(Latitude.unsafeMake(avgLatVal.degrees), Longitude.unsafeMake(finalNormalizedLonDegrees.degrees))
    }

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

    override def toString: String = s"[$p1 , $p2 , $p3]" // p1.toString + " , " + p2.toString + " , " + p3.toString

    // Some basic geometry that I will need to revisit.
    override def area: Area =
      if p1 == p2 || p1 == p3 || p2 == p3 then SquareMeters(0)
      else
        // Convert latitudes and longitudes to radians for calculations
        val lat1_rad = p1.latitude.unwrap.toRadians
        val lon1_rad = p1.longitude.unwrap.toRadians
        val lat2_rad = p2.latitude.unwrap.toRadians
        val lon2_rad = p2.longitude.unwrap.toRadians
        val lat3_rad = p3.latitude.unwrap.toRadians
        val lon3_rad = p3.longitude.unwrap.toRadians

        // Helper function for Haversine
        def haversine(theta: Double): Double = sin(theta / 2) * sin(theta / 2)

        // Calculate side lengths (a, b, c) as angular distances (d/R) using Haversine formula
        def angularDistance(ptA_lat: Double, ptA_lon: Double, ptB_lat: Double, ptB_lon: Double): Double = {
          val dLat     = ptB_lat - ptA_lat
          val dLon     = ptB_lon - ptA_lon
          val hav_dLat = haversine(dLat)
          val hav_dLon = haversine(dLon)
          2 * asin(sqrt(hav_dLat + cos(ptA_lat) * cos(ptB_lat) * hav_dLon))
        }

        val ang_a = angularDistance(lat2_rad, lon2_rad, lat3_rad, lon3_rad) // side a: p2 to p3
        val ang_b = angularDistance(lat1_rad, lon1_rad, lat3_rad, lon3_rad) // side b: p1 to p3
        val ang_c = angularDistance(lat1_rad, lon1_rad, lat2_rad, lon2_rad) // side c: p1 to p2

        // Check for collinearity: if one side is approx sum of other two (in radians)
        // This handles cases where points are distinct but lie on the same great circle arc.
        val sides               = List(ang_a, ang_b, ang_c).sorted
        val epsilonAngleRadians = 1e-8 // A small tolerance for floating point comparisons of angles in radians
        if abs(sides(2) - (sides(0) + sides(1))) < epsilonAngleRadians then {
          return SquareMeters(0) // Collinear or nearly collinear
        }

        // Calculate interior angles (alpha, beta, gamma) using spherical law of cosines
        // Ensure arguments to acos are within [-1, 1] due to potential floating point inaccuracies
        def calculateAngle(opposite_ang: Double, adj1_ang: Double, adj2_ang: Double): Double = {
          val cos_val = (cos(opposite_ang) - cos(adj1_ang) * cos(adj2_ang)) / (sin(adj1_ang) * sin(adj2_ang))
          acos(Math.max(-1.0, Math.min(1.0, cos_val))) // Clamp value to avoid NaN
        }

        val alpha = calculateAngle(ang_a, ang_b, ang_c) // Angle at p1
        val beta  = calculateAngle(ang_b, ang_a, ang_c) // Angle at p2
        val gamma = calculateAngle(ang_c, ang_a, ang_b) // Angle at p3

        // Spherical Excess (E)
        val sphericalExcess = alpha + beta + gamma - PI // JPI is java.lang.Math.PI

        // Area = E * R^2
        // R is Coordinates.R which is Length (e.g., Kilometers(6372.8))
        // R * R gives Area (e.g., SquareKilometers)
        val areaOnSphere = (R * R) * sphericalExcess // Squants: Area * Double = Area
        SquareMeters(areaOnSphere) // Convert to SquareMeters

//        val lat1 = p1.latitude.unwrap.toDegrees
//        val lon1 = p1.longitude.unwrap.toDegrees
//        val lat2 = p2.latitude.unwrap.toDegrees
//        val lon2 = p2.longitude.unwrap.toDegrees
//        val lat3 = p3.latitude.unwrap.toDegrees
//        val lon3 = p3.longitude.unwrap.toDegrees
//
//        // Using the shoelace formula for area calculation
//        val areaValue = R * R * abs(
//          lat1 * lon2 +
//          lat2 * lon3 +
//          lat3 * lon1 -
//          lat1 * lon3 -
//          lat2 * lon1 -
//          lat3 * lon2
//        )
//        SquareMeters(areaValue)

    // Distance will be used to link to specific stations
    // We currently use the distance to the barycenter
    override def distance(pt: Point): Length = barycenter.distance(pt)

//    override def distanceSquared(pt: Point): Double =
//      val dist = barycenter.distance(pt)
//      dist * dist

    def sorted: Triangle =
      val points = List(p1, p2, p3).sorted // Relies on implicit Ordering[Point] from TypeclassInstances
      Triangle(points(0), points(1), points(2))

    def isDegenerate: Boolean = p1 == p2 || p1 == p3 || p2 == p3

    /** Creates child triangles from a triangle
      */
    def midTriangles: List[Triangle] =
      List(
        Triangle(p1, p1.middle(p2), p1.middle(p3)),
        Triangle(p2, p2.middle(p1), p2.middle(p3)),
        Triangle(p3, p3.middle(p2), p3.middle(p1)),
        Triangle(p1.middle(p2), p2.middle(p3), p3.middle(p1))
      )

//    This partition is legacy, probably i will not need it anymore.
//    def partition(level: Int, puntosTriangulacion: Array[Point.PointKey]): List[Triangle] =

//    def maxsize: Boolean =
//      val containsPoint = puntosTriangulacion.contains(t.p1.keyCoordinates) ||
//        puntosTriangulacion.contains(t.p2.keyCoordinates) ||
//        puntosTriangulacion.contains(t.p3.keyCoordinates)
//
//      // Calculate distances between all points
//      val d1 = t.p1.distance(t.p2).toKilometers
//      val d2 = t.p1.distance(t.p3).toKilometers
//      val d3 = t.p2.distance(t.p3).toKilometers
//
//      // Find the maximum distance
//      val maxDistance = d1.max(d2).max(d3)
//
//      containsPoint && (maxDistance > 10.0)

    /** Creates child triangles from a parent triangle
      */
    def partition(level: Int): List[Triangle] =
      @tailrec
      def generateNthGeneration(remainingLevels: Int, currentGenerationTriangles: List[Triangle]): List[Triangle] =
        if remainingLevels <= 0 || currentGenerationTriangles.isEmpty then {
          currentGenerationTriangles
        } else {
          val nextGeneration = currentGenerationTriangles.flatMap(_.midTriangles)
          generateNthGeneration(remainingLevels - 1, nextGeneration)
        }

      if level < 0 then Nil // Or consider throwing an IllegalArgumentException for negative levels
      else if level == 0 then List(this)
      else generateNthGeneration(level, List(this))
  end Triangle

  object Triangle:
    /** Creates a degenerate triangle where all three vertices are the same point.
      */
    def apply(p: Point): Triangle = Triangle(p, p, p)

    /** Equality instance for Triangle that uses structural equality. Two triangles are considered equal if they have
      * the same three points, regardless of the order of the points.
      */
    given Equiv[Triangle] = Equiv.fromFunction { (t1, t2) =>
      val t1Points = Set(t1.p1, t1.p2, t1.p3)
      val t2Points = Set(t2.p1, t2.p2, t2.p3)
      t1Points == t2Points
    }

    given CanEqual[Triangle, Triangle] = CanEqual.derived
  end Triangle
end Triangles
