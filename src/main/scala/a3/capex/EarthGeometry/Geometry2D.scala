package com.a3.geodesy

import com.a3.geodesy.Coordinates._
import com.a3.geodesy.Coordinates.{Latitude, Longitude, Longitude360, LongitudeKey}
import com.a3.geodesy.CoordinatesImplicits.{ denormalizeLongitude, given_Conversion_Angle_Longitude,
  given_Conversion_Angle_Latitude, given_Conversion_Longitude_Angle, given_Conversion_Latitude_Angle,
  given_Conversion_Longitude360_Angle, given_Conversion_Longitude360_Longitude,
  given_Conversion_Longitude_Longitude360, given_Conversion_Long_LongitudeKey, given_Conversion_Long_LatitudeKey
}

import squants._
import squants.space.{Angle, Degrees, Kilometers, SquareMeters}
import squants.space.AngleConversions._
import squants.space.LengthConversions.LengthNumeric
import java.lang.Math._
import scalaz._
import scalaz.Scalaz._
import neotype._

object ShapesCore:

  /**
   * We will create an abstract class to act as root for any geometry we need.
   * It has some basic geometry built on.
   * It also provides some quick functions to help with things like intersections.
   */

  // This we use to fix the number of decimals in latitude longitude
  val angPrecission =  10000.0 // We take 4 decimals of degree, that is a maximum 14 m rounding
  // val gridPrecission = Meters(100)   // We take 2 decimals of degree, that is a maximum 1.4 km grid
  val gridPrecission = 10.0   // We take 1 decimals of degree, that is a maximum 14 km grid

  trait CurvedShapes extends Serializable:

    def north: Latitude
    def south: Latitude
    def east: Longitude
    def west: Longitude

    // We need these to handle east/west more easily
    def east360: Longitude360 = east
    def west360: Longitude360 = west

    def barycenter: Point

    def width: Angle = east360  - west360
    def height: Angle = north - south

    // Need to find a good reference for Spherical trigonometry
    def area: Area = width.toDegrees * height.toDegrees * SquareMeters(1)

    /**
     * Convert this CurvedShapes to a Box.
     *
     * For Boxes this is a no-op. For points, it constructs a box with
     * width and height of zero.
     */
    def toBox: Box = Box(Point(north, east), Point(south, west))

    /**
     * Distance between this geometry and the given point using harversine.
     *
     * The distance is measured in terms of the closest point in the
     * geometry. For points this is obvious (there is only one point to
     * use). For boxes, this means that points contained within the box
     * (or on the perimeter) have a distance of zero.
     */
    def distance(pt: Point): Length =
      val dLon: Double = if (pt.longitude < east) (pt.longitude - east).toRadians else if (pt.longitude < west) 0F else (west - pt.longitude).toRadians
      val dLat: Double = if (pt.latitude < south) (pt.latitude - south).toRadians else if (pt.latitude < north) 0F else (north - pt.latitude).toRadians
      val a = pow(sin(dLat / 2.0), 2) + pow(sin(dLon / 2.0), 2) * cos(north.toRadians) * cos(pt.latitude.toRadians)
      val c = 2 * asin(sqrt(a))
      R * c

    /**
     * Squared distance should be used when we are interested in ordering, we trade the actual value for spped,
     * while retaining the ranking.
     */
    def distanceSquared(pt: Point): Double =
      val dx: Angle = if (pt.longitude < east) east - pt.longitude else if (pt.longitude < west) zero else pt.longitude - west
      val dy: Angle = if (pt.latitude < south) south - pt.latitude else if (pt.latitude < north) zero else pt.latitude - north
      dx.toRadians * dx.toRadians + dy.toRadians * dy.toRadians

    /**
     * Get the lower-left (southwest) bound of the geometry.
     */
    def southWestCorner: (Latitude, Longitude) = (south, west)

    /**
     * Get the upper-right (northeast) bound of the geometry.
     */
    def northEastCorner: (Latitude, Longitude) = (north, east)

    /**
     * Returns whether this geometry contains the other.
     *
     * Containment includes the border, so points "on the edge" count as
     * contained.
     */
    def contains(geom: Box ): Boolean =
      east <= geom.east360 && geom.west360 <= west360 && south <= geom.south && geom.north <= north
    //Box | Point
    //      val geom
//      match
//      case Box(b)
//      => (east <= b.east360 && b.west360 <= west360 && south <= b.south && b.north <= north)
//      case Point(p)
//      => (east <= b.east360 && b.west360 <= west360 && south <= b.south && b.north <= north)

    /**
     * Return the given geometry's area outside this geometry.
     * This is equivalent to the area that would be added by expand().
     * Calculation is obviously wrong, need check formulas for this.
     */
    def expandArea(geom: Box): Area =
      val px1 = east.min(geom.east)
      val py1 = south.min(geom.south)
      val px2 = west.max(geom.west)
      val py2 = north.max(geom.north)
      val a = (py2.toDegrees - py1.toDegrees) * (px2.toDegrees - px1.toDegrees) * SquareMeters(1)
      a - area

    /**
     * Construct a new Location that contains this geometry and another.
     *
     * This will be the smallest possible box. The result of this method
     * is guaranteed to contain both geometries.
     */
    def expand(geom: Box): Box =
      Box(Point(Latitude.unsafeMake(south.min(geom.south)), denormalizeLongitude(east360.min(geom.east360))),
        Point(Latitude.unsafeMake(north.max(geom.north)), denormalizeLongitude(west360.max(geom.west360))))

    /**
     * Returns whether this geometry intersects with the other.
     *
     * Intersection includes the border, so points "on the edge" count
     * as intersecting.
     */
    def intersects(geom: Box): Boolean =
      (east <= geom.east && geom.east <= west && south <= geom.south && geom.south <= north) ||
        (east <= geom.west && geom.west <= west && south <= geom.south && geom.south <= north) ||
        (east <= geom.east && geom.east <= west && south <= geom.north && geom.north <= north) ||
        (east <= geom.west && geom.west <= west && south <= geom.north && geom.north <= north)

    /**
     * Returns whether this geometry wraps the other.
     *
     * This is the same thing as containment, but it excludes the
     * border. Points can never wrap anything, and boxes can only wrap
     * geometries with less area than they have.
     */
    def wraps(geom: Box): Boolean =
      east < geom.east360 && geom.west360 < west360 && south < geom.south && geom.north < north


  case class Point(latitude: Latitude, longitude: Longitude) extends CurvedShapes:
    override def barycenter = this
    override def north = latitude
    override def south = latitude
    override def east = longitude
    override def west = longitude
    override def width: Angle = zero
    override def height: Angle = zero
    override def area: Area = SquareMeters(0)
    override def toString: String = s"($latitude , $longitude)"  // "(" + + ")"

    override def distance(pt: Point): Length =
      val dLon = pt.longitude - longitude
      val dLat = pt.latitude - latitude
      val a = pow((dLat / 2).sin, 2) + pow((dLon / 2).sin, 2) * latitude.cos * pt.latitude.cos
      val c = 2 * asin(sqrt(a))
      R * c

    override def distanceSquared(pt: Point): Double =
      val dx: Angle = longitude - pt.longitude
      val dy: Angle = latitude - pt.latitude
      dx.toRadians * dx.toRadians + dy.toRadians * dy.toRadians

    // A point can not contain anything.
    override def contains(geom: Box) = false

    def sorted(p: Point): (Point, Point) =
      if (this.isSorted(p)) (this, p) else (p, this)

    // Sorted by latitude, used to ensure membership of edges
    def isSorted(p: Point): Boolean =
      if ((latitude < p.latitude) || (latitude == p.latitude && longitude < p.longitude)) true else false

    // Middle point
    def middle(p: Point): Point =
      Point((latitude + p.latitude) / 2, (longitude + p.longitude) / 2)

    def keyCoordinates: Point.PointKey = Point.PointKey(
     (angPrecission * longitude.toDegrees).toLong, (angPrecission * latitude.toDegrees).toLong)

    def gridCoordinates: Point.PointKey = Point.PointKey((gridPrecission * latitude.toDegrees).toLong,
      (gridPrecission * longitude.toDegrees).toLong)

    // Calculate the angle to another point as azimuth, ie. angle from the South-North axis
    def azimuthTo(another: Point): Angle =
      implicit val precision: Angle = 0.0001.degrees
      if (this.keyCoordinates == another.keyCoordinates)
        0.degrees
      else if (this.longitude =~ another.longitude)
        //if (printme) println("mismalongitud")
        if (this.latitude < another.latitude) 0.degrees else 180.degrees
      else if (this.latitude =~ another.latitude)
        //if (printme) println("mismalongitud")
        if (this.longitude < another.longitude) 90.degrees else -90.degrees
      else
        //        val absoluteAngle = math.atan(math.abs((another.longitude - this.longitude).toRadians) / math.abs((another.latitude - this.latitude).toRadians)).radians
        //        //if (printme) println(absoluteAngle)
        val deltaLat = another.latitude - this.latitude
        val deltaLong = another.longitude - this.longitude
        //        if (deltaLat > 0.degrees && deltaLong > 0.degrees) {
        //          absoluteAngle
        //        } else if (deltaLong > 0.degrees && deltaLat < 0.degrees) {
        //          //if (printme) println("mas 90")
        //          90.degrees + absoluteAngle
        //        } else if (deltaLong < 0.degrees && deltaLat < 0.degrees) {
        //          //         if (printme) println("mas 180")
        //          180.degrees + absoluteAngle
        //        } else {
        // //         if (printme) println("mas 270")
        //          270.degrees + absoluteAngle
        //        }
        val t = math.atan2(deltaLong.value, deltaLat.value).radians.toDegrees
        // if (printme) println(s"${this} to ${another}, ${deltaLong.value} ${deltaLat.value} ${t} or ${if (t < 0.0)  (360.0 + t) else t}")
        (if (t < 0.0) (360.0 + t) else t).degrees


  object Point:
    val zeroPoint = Point(Latitude.unsafeMake(zero), Longitude.unsafeMake(zero))
    val lon: Option[LongitudeKey] = None
    val lat: Option[LongitudeKey] = None
    // With this we should be able to use points as keys
    // Should this be part of an object or a normal case class , and why TODO
    case class PointGrid(latitude: LatitudeKey, longitude: LongitudeKey)
    case class PointKey(latitude: LatitudeKey, longitude: LongitudeKey)

    object PointKey:
      implicit val equal: Equal[PointKey] = Equal.equalA

  object PointImplicits:
    given Conversion[Point, Point.PointKey] = _.keyCoordinates
    given Conversion[Point, Box] = _.toBox


//      implicit class PointKey2PointVal(val value: Point.PointKey) extends AnyVal:
//      def toPoint: Point = Point(Degrees(value.latitude / angPrecission), Degrees(value.longitude / angPrecission))
//      case class PointKey(latitude: LatitudeKey, longitude: LongitudeKey):
//        //      lon = Option(longitude)
//        //      lat = Option(latitude)
//        def gridCoordinates: Point.PointGrid =
//          Point.PointGrid((gridPrecission * latitude / angPrecission).toLong, (gridPrecission * longitude / angPrecission).toLong)

//  def gridCoordinates: Point.PointGrid = Point.PointKey((gridPrecission * latitude.toDegrees).toLong,
//    (gridPrecission * longitude.toDegrees).toLong)

  case class Box(northEast: Point, southWest: Point) extends CurvedShapes:
//    override def toBox: Box = this
    override def east = northEast.longitude
    override def north = northEast.latitude
    override def west = southWest.longitude
    override def south = southWest.latitude
    override def barycenter = Point((north + south) / 2.0, denormalizeLongitude((east360 + west360) / 2.0))

  object Box:
    /**
     * This is an "inside-out" box that we use as a good starting
     * value. The nice thing about this, unlike Box(0,0,0,0), is that
     * when merging with another box we don't include an artifical
     * "empty" point.
     */
    val empty: Box =
      val s = Math.sqrt(Double.MaxValue).degrees
      val t = (s + -2.0F * s).degrees
      Box(Point(s, s), Point(t, t))
