package com.a3.capex.geodesy

import Coordinates.{Latitude, Longitude, zero, R}
import TypeclassInstances.given
import squants.space.{Angle, Area, Length, SquareMeters}
import squants.space.AngleConversions.*


import java.lang.Math.*
import neotype.*
import scala.language.implicitConversions
import com.a3.capex.geodesy.CoordinatesImplicits.given_Conversion_Long_LatitudeKey
import com.a3.capex.geodesy.CoordinatesImplicits.given_Conversion_Long_LongitudeKey

// Define type aliases for the opaque types
type LatitudeKey = Long
type LongitudeKey = Long

object ShapesCore:
  /**
   * We will create an abstract class to act as root for any geometry we need.
   * It has some basic geometry built on.
   * It also provides some quick functions to help with things like intersections.
   */
  val angPrecission =  10000.0 // We take 4 decimals of degree, that is a maximum 14 m rounding
  val gridPrecission = 10.0    // We take 1 decimals of degree, that is a maximum 14 km grid

  trait CurvedShapes extends Serializable:

    def north: Latitude
    def south: Latitude
    def east: Longitude
    def west: Longitude
    def width: Angle = {
      val eDeg = east.unwrap.toDegrees
      val wDeg = west.unwrap.toDegrees
      val directSpan = Math.abs(eDeg - wDeg)
      val widthInDegrees = Math.min(directSpan, 360.0 - directSpan)
      widthInDegrees.degrees
    }
    def height: Angle = north - south
    def barycenter: Point
    def isDegenerate: Boolean

    // Need to find a good reference for Spherical trigonometry
    def area: Area = SquareMeters(1) * (width.toDegrees * height.toDegrees)

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
      val dLon: Double = if (pt.longitude < east) (pt.longitude - east).toRadians
      else if (pt.longitude < west) 0F
      else (west - pt.longitude).toRadians
      val dLat: Double = if (pt.latitude < south) (pt.latitude - south).toRadians
      else if (pt.latitude < north) 0F
      else (north - pt.latitude).toRadians
      val a = pow(sin(dLat / 2.0), 2) + pow(sin(dLon / 2.0), 2) * cos(north.unwrap.toRadians) * cos(pt.latitude.unwrap.toRadians)
      val c = 2 * asin(sqrt(a))
      R * c

    /**
     * Squared distance should be used when we are interested in ordering, we trade the actual value for spped,
     * while retaining the ranking.
     */
    def distanceSquared(pt: Point): Double =
      val dx: Angle = if (pt.longitude < east) east - pt.longitude
      else if (pt.longitude < west) zero
      else pt.longitude - west
      val dy: Angle = if (pt.latitude < south) south - pt.latitude
      else if (pt.latitude < north) zero
      else pt.latitude - north
      dx.toRadians * dx.toRadians + dy.toRadians * dy.toRadians

    /**
     * Get the lower-left (southwest) bound of the geometry.
     */
    def southWestCorner: (Latitude, Longitude) = (south, west)

    /**
     * Get the upper-right (northeast) bound of the geometry.
     */
    def northEastCorner: (Latitude, Longitude) = (north, east)

    // HELPER METHODS for contains, using "smaller span" logic
    private def getEffectiveLongitudeSpan(boxEast: Longitude, boxWest: Longitude): (Double, Double, Boolean) = {
      val e = boxEast.unwrap.toDegrees
      val w = boxWest.unwrap.toDegrees

      if (e == w) { // Point or meridian line
        (w, e, false)
      } else {
        // spanWestToEastClockwise is the angular distance from W to E moving eastward
        val spanWestToEastClockwise = (e - w + 360.0) % 360.0
        
        if (spanWestToEastClockwise <= 180.0) {
          // This is the shorter span (or one of two equal 180deg spans).
          // It goes from W to E (clockwise).
          // It crosses the Antimeridian if E is numerically smaller than W (e.g. W=350, E=10).
          (w, e, e < w)
        } else {
          // The other span (E to W, clockwise) is shorter.
          // Effective W is E, effective E is W.
          // It crosses the Antimeridian if W is numerically smaller than E (e.g. W=10, E=350 implies effective W=350, E=10).
          (e, w, w < e)
        }
      }
    }

    private def isLongitudeInsideEffectiveSpan(lon: Longitude, effWest: Double, effEast: Double, effCrossesAM: Boolean): Boolean = {
      val lonDeg = lon.unwrap.toDegrees
      if (effCrossesAM) {
        // For an AM-crossing span, longitude is inside if it's on either side of AM, within the span parts.
        lonDeg >= effWest || lonDeg <= effEast
      } else {
        // For a standard span, simple range check.
        // Handles the case where effWest == effEast (meridian line) correctly.
        lonDeg >= effWest && lonDeg <= effEast
      }
    }

    /**
     * Returns whether this geometry contains the other.
     *
     * Containment includes the border, so points "on the edge" count as
     * contained.
     * This logic now considers the "smaller span" interpretation for longitude ranges.
     */
    def contains(geom: Box ): Boolean = {
      // Get effective longitude spans based on "smaller span" rule
      val (thisEffW, thisEffE, thisCrossesAM) = getEffectiveLongitudeSpan(this.east, this.west)
      val (geomEffW, geomEffE, geomCrossesAM) = getEffectiveLongitudeSpan(geom.east, geom.west)

//      // Debugging output
//      println(s"[Box.contains] === Checking if THIS box: ${this} contains OTHER box: ${geom} ===")
//      println(s"[Box.contains] THIS (raw): N=${this.north.unwrap.toDegrees}, S=${this.south.unwrap.toDegrees}, E=${this.east.unwrap.toDegrees}, W=${this.west.unwrap.toDegrees}")
//      println(s"[Box.contains] THIS (eff): effW=${thisEffW}, effE=${thisEffE}, crossesAM=${thisCrossesAM}, Width=${this.width.toDegrees}")
//      println(s"[Box.contains] OTHER (raw): N=${geom.north.unwrap.toDegrees}, S=${geom.south.unwrap.toDegrees}, E=${geom.east.unwrap.toDegrees}, W=${geom.west.unwrap.toDegrees}")
//      println(s"[Box.contains] OTHER (eff): effW=${geomEffW}, effE=${geomEffE}, crossesAM=${geomCrossesAM}, Width=${geom.width.toDegrees}")
//
      val latContains = (this.north >= geom.north) && (this.south <= geom.south)
//      println(s"[Box.contains] Latitudinal containment: ${latContains}")

      // Longitudinal checks using effective spans for 'this' and raw points for 'geom'
      val geomWestInsideThis = isLongitudeInsideEffectiveSpan(geom.west, thisEffW, thisEffE, thisCrossesAM)
//      println(s"  [lonCheck] geom.west_raw (${geom.west.unwrap.toDegrees}) inside THIS_eff (W:$thisEffW, E:$thisEffE, AM:$thisCrossesAM): $geomWestInsideThis")
      
      val geomEastInsideThis = isLongitudeInsideEffectiveSpan(geom.east, thisEffW, thisEffE, thisCrossesAM)
//      println(s"  [lonCheck] geom.east_raw (${geom.east.unwrap.toDegrees}) inside THIS_eff (W:$thisEffW, E:$thisEffE, AM:$thisCrossesAM): $geomEastInsideThis")
      
      // If 'this' effective span is standard but 'geom's effective span crosses AM, 'geom' cannot be contained.
      val longitudinalSpanContained = if (!thisCrossesAM && geomCrossesAM)
//        println(s"  [lonCheck] Longitudinal span: FALSE (standard 'this_eff' cannot contain AM-crossing 'geom_eff')")
        false
      else
        // Check if the defining west and east points of the geom Box fall within this Box's effective span.
        val result = geomWestInsideThis && geomEastInsideThis
//        println(s"  [lonCheck] Longitudinal span (geom.west_raw & geom.east_raw in THIS_eff): $result")
        result

      // Width condition using a small tolerance for floating point comparison
      val widthCondition = (this.width.toDegrees >= geom.width.toDegrees - 1e-9)
//      println(s"[Box.contains] Width condition (this.width (${this.width.toDegrees}) >= geom.width (${geom.width.toDegrees})): ${widthCondition}")

      val finalResult = latContains && longitudinalSpanContained && widthCondition
//      println(s"[Box.contains] Final result: ${finalResult} (lat:${latContains} && lonSpan:${longitudinalSpanContained} && width:${widthCondition})")
//      println(s"[Box.contains] =======================================================================")
      finalResult
    }
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
      val px1 = east.min(geom.east).unwrap.toDegrees
      val py1 = south.min(geom.south).unwrap.toDegrees
      val px2 = west.max(geom.west).unwrap.toDegrees
      val py2 = north.max(geom.north).unwrap.toDegrees
      val a = (py2 - py1) * (px2 - px1)
      SquareMeters(1)*a - area

    /**
     * Construct a new Location that contains this geometry and another.
     *
     * This will be the smallest possible box. The result of this method
     * is guaranteed to contain both geometries.
     */
    def expand(geom: Box): Box =
      val newNorth = this.north.max(geom.north)
      val newSouth = this.south.min(geom.south)
      val newEast  = this.east.max(geom.east)   // Longitude.max handles antimeridian logic
      val newWest  = this.west.min(geom.west)   // Longitude.min handles antimeridian logic
      Box(Point(newNorth, newEast), Point(newSouth, newWest))

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
      east < geom.east && geom.west < west && south < geom.south && geom.north < north


  case class Point(latitude: Latitude, longitude: Longitude) extends CurvedShapes:
    override def barycenter: Point = this
    override def north: Latitude = latitude
    override def south: Latitude = latitude
    override def east: Longitude = longitude
    override def west: Longitude = longitude
    override def width: Angle = zero
    override def height: Angle = zero
    override def area: Area = SquareMeters(0)
    override def toString: String = s"($latitude , $longitude)"  // "(" + + ")"
    override def isDegenerate: Boolean = false

    def + (other: Point): Point =
      val lat: Angle = (this.latitude.unwrap + other.latitude.unwrap) % 360
      val lon: Angle = (this.longitude.unwrap + other.longitude.unwrap) % 360
      Point(Latitude.unsafeMake(lat), Longitude.unsafeMake(lon))

    def / (other: Double): Point =
      val lat: Angle = (this.latitude.unwrap.toDegrees/other).degrees
      val lon: Angle = (this.longitude.unwrap.toDegrees/other).degrees
      Point(Latitude.unsafeMake(lat), Longitude.unsafeMake(lon))

    def * (other: Double): Point.PointKey = Point.PointKey( (other * this.latitude.unwrap.toDegrees).toLong,
      (other * this.longitude.unwrap.toDegrees).toLong)

    override def distance(pt: Point): Length =
      val dLon = pt.longitude - longitude
      val dLat = pt.latitude - latitude
      val a = pow((dLat / 2).sin, 2) + pow((dLon / 2).sin, 2) * cos(latitude.unwrap.toRadians) * cos(pt.latitude.unwrap.toRadians)
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
      Point(latitude + p.latitude, longitude + p.longitude) / 2

    def keyCoordinates: Point.PointKey = this * angPrecission

    def gridCoordinates: Point.PointKey = this * gridPrecission

    // Calculate the angle to another point as azimuth, ie. angle from the South-North axis
    def azimuthTo(another: Point): Angle =
      implicit val precision: Angle = 0.0001.degrees
      if (this.keyCoordinates == another.keyCoordinates)
        0.degrees
      else if (this.longitude.unwrap =~ another.longitude.unwrap)
        //if (printme) println("mismalongitud")
        if (this.latitude < another.latitude) 0.degrees else 180.degrees
      else if (this.latitude.unwrap =~ another.latitude.unwrap)
        //if (printme) println("mismalongitud")
        if (this.longitude < another.longitude) 90.degrees else -90.degrees
      else
        //        val absoluteAngle = math.atan(math.abs((another.longitude - this.longitude).toRadians) /
        //          math.abs((another.latitude - this.latitude).toRadians)).radians
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
        // if (printme) println(s"${this} to ${another}, ${deltaLong.value} ${deltaLat.value} ${t} or
        //   ${if (t < 0.0)  (360.0 + t) else t}")
        (if (t < 0.0) t + 360.0 else t ).degrees


  object Point:

    val zeroPoint: Point = Point(Latitude.unsafeMake(zero), Longitude.unsafeMake(zero))
    val lon: Option[Coordinates.LongitudeKey] = None
    val lat: Option[Coordinates.LongitudeKey] = None
    // With this we should be able to use points as keys
    // Should this be part of an object or a normal case class , and why TODO
    case class PointGrid(latitude: Coordinates.LatitudeKey, longitude: Coordinates.LongitudeKey)
    case class PointKey(latitude: Coordinates.LatitudeKey, longitude: Coordinates.LongitudeKey)

    def onSegment(p: Point, q: Point, r: Point): Boolean =
      orientation(p, q, r) == 0 && // Check for collinearity first
        q.latitude <= p.latitude.max(r.latitude) &&
        q.latitude >= p.latitude.min(r.latitude) &&
        q.longitude <= p.longitude.max(r.longitude) &&
        q.longitude >= p.longitude.min(r.longitude)

    def orientation(p: Point, q: Point, r: Point): Int =
      val value = (q.longitude.unwrap.toDegrees - p.longitude.unwrap.toDegrees) *
        (r.latitude.unwrap.toDegrees - q.latitude.unwrap.toDegrees) -
        (q.latitude.unwrap.toDegrees - p.latitude.unwrap.toDegrees) *
          (r.longitude.unwrap.toDegrees - q.longitude.unwrap.toDegrees)

      if (value == 0) 0 // Collinear
      else if (value > 0) 1 // Clockwise
      else 2 // Counterclockwise


    object PointKey:
      // Equality instance for PointKey
      given CanEqual[PointKey, PointKey] = CanEqual.derived
      given Equiv[PointKey] = Equiv.fromFunction(_ == _)


  object PointImplicits:
    given Conversion[Point, Point.PointKey] = _.keyCoordinates
    given Conversion[Point, Box] = _.toBox

  //      implicit class PointKey2PointVal(val value: Point.PointKey) extends AnyVal:
  //      def toPoint: Point = Point(Degrees(value.latitude / angPrecission), Degrees(value.longitude / angPrecission))
  //      case class PointKey(latitude: LatitudeKey, longitude: LongitudeKey):
  //        //      lon = Option(longitude)
  //        //      lat = Option(latitude)
  //        def gridCoordinates: Point.PointGrid =
  //          Point.PointGrid((gridPrecission * latitude / angPrecission).toLong, (gridPrecission * longitude /
  //            angPrecission).toLong)

  //  def gridCoordinates: Point.PointGrid = Point.PointKey((gridPrecission * latitude.toDegrees).toLong,
  //    (gridPrecission * longitude.toDegrees).toLong)

  case class Box(northEast: Point, southWest: Point) extends CurvedShapes:
    override def toBox: Box = this
    override def east: Longitude = northEast.longitude
    override def north: Latitude = northEast.latitude.max(southWest.latitude)
    override def west: Longitude = southWest.longitude
    override def south: Latitude = northEast.latitude.min(southWest.latitude)
    override def barycenter: Point = (northEast + southWest)/2.0 // Point((north + south) / 2.0, ((east + west) / 2.0))
    override def isDegenerate: Boolean = northEast == southWest

  // Is this actually needed anymore
  object Box:
    /**
     * This is an "inside-out" box that we use as a good starting
     * value. The nice thing about this, unlike Box(0,0,0,0), is that
     * when merging with another box we don't include an artificial
     * "empty" point.
     */
    val empty: Box =
      val s = Math.sqrt(Double.MaxValue).degrees
      val lat1 = Latitude.unsafeMake(s)
      val lat2 = Latitude.unsafeMake(-s)
      val lon1 = Longitude.unsafeMake(s)
      val lon2 = Longitude.unsafeMake(-s)
      Box(Point(lat1, lon1), Point(lat2, lon2))