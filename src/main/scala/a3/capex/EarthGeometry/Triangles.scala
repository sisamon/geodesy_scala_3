package org.a3.geodesy

import com.a3.geodesy.Coordinates._
import com.a3.geodesy.CoordinatesImplicits.{
  given_Conversion_Longitude_Angle, given_Conversion_Latitude_Angle,
  given_Conversion_Angle_Longitude, given_Conversion_Angle_Latitude
}
import com.a3.geodesy.ShapesCore._

import squants._
import squants.space.{Angle, Degrees, Kilometers, SquareMeters}
import squants.space.AngleConversions._
import squants.space.AngleConversions.AngleConversions
import squants.space.LengthConversions.LengthNumeric
import java.lang.Math._
import scala.annotation.tailrec
import scala.math.sqrt
import scalaz._
import scalaz.Scalaz._
import neotype._


object Triangles:
  //      val px1 = normalizeLongitude(longitude1).min(normalizeLongitude(geom.longitude1))
  //      val py1 = latitude1.unwrap.min(geom.latitude1.unwrap)
  //      val px2 = normalizeLongitude(longitude2).max(normalizeLongitude(geom.longitude2))
  //      val py2 = latitude2.unwrap.max(geom.latitude2.unwrap)
  //      val a = (py2.toDegrees - py1.toDegrees) * (px2.toDegrees - px1.toDegrees) * SquareMeters(1)
  //      a - area
  //

  // This is magic stuff
  implicit val angleMonoid: Monoid[Angle] = Monoid.instance((a, b) => (a.toDegrees + b.toDegrees).degrees, zero)
  implicit val lengthMonoid: Monoid[Length] = Monoid.instance((a, b) => a + b, Meters(0))
  implicit val longitudeMonoid: Monoid[Longitude] =
    Monoid.instance((a, b) => Longitude.unsafeMake((a.unwrap.toDegrees + b.unwrap.toDegrees).degrees), Longitude.unsafeMake(zero))
  implicit val latitudeMonoid: Monoid[Latitude] =
    Monoid.instance((a, b) => Latitude.unsafeMake((a.unwrap.toDegrees + b.unwrap.toDegrees).degrees), Latitude.unsafeMake(zero))

  implicit val angleOrder: Order[Angle] = Order.fromScalaOrdering[Angle]
  implicit val lengthOrder: Order[Length] = Order.fromScalaOrdering[Length]

  // These two are not working...
  //  implicit val longitudeOrder: Order[Longitude] = Order.fromScalaOrdering[Longitude]
  //  implicit val latitudeOrder: Order[Latitude] = Order.fromScalaOrdering[Latitude]
  case class Triangle(p1: Point, p2: Point, p3: Point) extends CurvedShapes:
    override def barycenter: Point = Point(Latitude.unsafeMake((p1.latitude + p2.latitude + p3.latitude)/3),
        Longitude.unsafeMake((p1.longitude + p2.longitude + p3.longitude) / 3))

    // I need to unwrap because I can not define an Ordering
    override def east: com.a3.geodesy.Coordinates.Longitude =
      ~List(p1.longitude.unwrap, p2.longitude.unwrap, p3.longitude.unwrap).minimum
// No given instance of type scalaz.Order[com.a3.geodesy.Coordinates.Longitude.Type] was found for parameter A of method minimum in class FoldableOps.
// I found:
//
//    scalaz.Scalaz.enumInstance[E]
//
// But method enumInstance in trait EnumInstances does not match type scalaz.Order[com.a3.geodesy.Coordinates.Longitude.Type].
//      ~List(p1.longitude, p2.longitude, p3.longitude).minimum

    override def north: com.a3.geodesy.Coordinates.Latitude =
      ~List(p1.latitude.unwrap, p2.latitude.unwrap, p3.latitude.unwrap).maximum
    override def south: com.a3.geodesy.Coordinates.Latitude =
      ~List(p1.latitude.unwrap, p2.latitude.unwrap, p3.latitude.unwrap).minimum
    override def west: com.a3.geodesy.Coordinates.Longitude =
      ~List(p1.longitude.unwrap, p2.longitude.unwrap, p3.longitude.unwrap).maximum

    override def toString: String = s"[$p1 , $p2 , $p3]"  //p1.toString + " , " + p2.toString + " , " + p3.toString

    // Some basic geometry that i will need to revisit.
    override def area: Area =
      if (p1 == p2 || p1 == p3 || p2 == p3) then
        R * R * abs(p1.latitude.toDegrees * p2.longitude.toDegrees + p2.latitude.toDegrees * p3.longitude.toDegrees +
          p3.latitude.toDegrees * p1.longitude.toDegrees - p1.latitude.toDegrees * p3.longitude.toDegrees -
          p2.latitude.toDegrees * p1.longitude.toDegrees - p3.latitude.toDegrees * p2.longitude.toDegrees)
      else
        SquareMeters(0)

    // Distance will be used to link to specific stations
    // We currently use the distance to the barycenter
    override def distance(pt: Point): Length = barycenter.distance(pt)

    override def distanceSquared(pt: Point): Double = barycenter.distanceSquared(pt)

    def sorted: Triangle =
      if (p1.isSorted(p2) && p1.isSorted(p3) && p2.isSorted(p3)) Triangle(p1, p2, p3)
      else if (p1.isSorted(p2) && p1.isSorted(p3) && p3.isSorted(p2)) Triangle(p1, p3, p2)
      else if (p2.isSorted(p1) && p2.isSorted(p3) && p1.isSorted(p3)) Triangle(p2, p1, p3)
      else if (p2.isSorted(p1) && p2.isSorted(p3) && p3.isSorted(p1)) Triangle(p2, p3, p1)
      else if (p3.isSorted(p1) && p3.isSorted(p2) && p1.isSorted(p2)) Triangle(p3, p2, p1)
      else Triangle(p3, p1, p2) // (p3.isSorted(p1) && p3.isSorted(p2) && p2.isSorted(p1))

    def isDegenerate: Boolean = (p1 == p2 || p1 == p3 || p2 == p3)

    def partition(level: Int, puntosTriangulacion: Array[Point.PointKey]): List[Triangle] =

      def midTriangles(t: Triangle): List[Triangle] =
        List(Triangle(t.p1, t.p1.middle(t.p2), t.p1.middle(t.p3)),
          Triangle(t.p2, t.p2.middle(t.p1), t.p2.middle(t.p3)),
          Triangle(t.p3, t.p3.middle(t.p2), t.p3.middle(t.p1)),
          Triangle(t.p1.middle(t.p2), t.p2.middle(t.p3), t.p3.middle(t.p1)))

      def maxsize(t: Triangle): Boolean =
        println(puntosTriangulacion.contains(t.p1.keyCoordinates) ||
          puntosTriangulacion.contains(t.p2.keyCoordinates) ||
          puntosTriangulacion.contains(t.p3.keyCoordinates))
        // println(s"Triangulo ${t.p1}, ${t.p2}, ${t.p3}")
        // puntosTriangulacion.take(5).foreach(println)
        (puntosTriangulacion.contains(t.p1.keyCoordinates) || puntosTriangulacion.contains(t.p2.keyCoordinates) || puntosTriangulacion.contains(t.p3.keyCoordinates)) &&
          List(t.p1.distance(t.p2), t.p1.distance(t.p3), t.p3.distance(t.p2)).max > Kilometers(10)

      @tailrec
      def generateChildren(level: Int, triangles: List[Triangle], children: List[Triangle]): List[Triangle] =
        // println("generating children")
        (level, triangles) match {
          case (_, Nil) =>
            children
          // case (1, x) if maxsize(x) =>
          //  generateChildren(1, x.flatMap(midTriangles), children)
          case (1, x) =>
            x ::: children
          case (n, x) =>
            generateChildren(n - 1, x.flatMap(midTriangles), children)
        }

      if (level >= 1) generateChildren(level, midTriangles(this), Nil)
      else Nil


  object Triangle:
    implicit val equal: Equal[Triangle] = Equal.equalA //[Triangle]

//
  //  final case class TriangleDegenerate(p: Point) extends TriangleTrait:
  //    def p1 = p
  //    def p2 = p
  //    def p3 = p