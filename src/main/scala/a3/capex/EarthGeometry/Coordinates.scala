package com.a3.geodesy

import scalaz.Scalaz.*
import scalaz.*
import squants.*
import squants.space.AngleConversions.*
import squants.space.{Angle, Degrees, Kilometers, SquareMeters}
import neotype.*
// I am going to get rid of squants, but for the time being...
//import quantitative.*

/**
 * @author Luis Sisamon
 */

/**
 *  We are going to work on decimal degrees, we can always add UTM or
 *  other coordinates systems, but it is simpler to define the logic 
 *  using this approach.
 */

object Coordinates:
  val R = Kilometers(6372.8) //radius in km

  val zero: Angle = Degrees(0.0)

/**
 *  Lets take advantage of Scalas's type system to avoid swapping lat and lon
 *  coordinates accidentally.
 *  This is also important because latitude goes from -90 to +90, while 
 *  longitude goes from -180 to 180. Besides that the behavior of latitude 
 *  is "normal", that is a lower latitude is always lower, south of, we will 
 *  never have any structure going from 85 to -85.
 *  For longitude is more ambiguous, a rectangle from 179 to -179 may happen 
 *  and we can not use the lower criteria to order west to east.
 *  Of course this may be an issue when transposing, but I see no use to
 *  transpose latitude and longitude.
 */

  type TypeValidation = Boolean | String

  def isBetween(num: Double, lower: Double, upper: Double): Boolean =
    lower <= upper && num >= lower && num <= upper

  /**
   * Our visible coordinate system will mimick the usaal used in geography.
   */
  type Longitude = Longitude.Type

  object Longitude extends Newtype[Angle]:
    override inline def validate(value: Angle): TypeValidation =
      if (value.toDegrees >= -180.0 && value.toDegrees <= 180.0)
        true
      else
        "Longitude must be between -180.0 and 180.0"

  type Latitude = Latitude.Type

  object Latitude extends Newtype[Angle]:
    override inline def validate(value: Angle): TypeValidation =
      if (value.toDegrees >= -90.0 && value.toDegrees < 90.0)
        true
      else
        "Latitude must be between -90.0 and 90.0"

  /**
   * To make things simpler we have longitude between 0 and 360
   * because this is breaking any standard practice we will keep this
   * hidden from our geometry.
   */

  type Longitude360 = Longitude360.Type

  object Longitude360 extends Newtype[Angle]:
    override inline def validate(value: Angle): TypeValidation =
      if (value.toDegrees >= 0.0 && value.toDegrees <= 360.0)
        true
      else
        "Longitude360 must be between 0.0 and 360.0"

  /**
   * These two types will be used to be able to use points as keys for maps and similar.
   */
  opaque type LongitudeKey = Long

  object LongitudeKey:
    def apply(l: Long) = l

  opaque type LatitudeKey = Long

  object LatitudeKey:
    def apply(l: Long) = l


object CoordinatesImplicits:

  import com.a3.geodesy.Coordinates.{Latitude, Longitude, Longitude360, LongitudeKey, LatitudeKey}

  /**
   * Turn the -180 to 180 into a more convenient 0 to 360, more convenient for calcualtions.
   */
  def normalizeLongitude(l: Longitude): Longitude360 =
    val d = l.unwrap
    Longitude360.unsafeMake( d+ (if (d > Degrees(0.0)) Degrees(0.0) else Degrees(360.0)) )

  /**
   * Turns back into the -180 to 180.
   */
  def denormalizeLongitude(d: Angle): Longitude =
    Longitude.unsafeMake(d + (if (d > Degrees(180.0)) Degrees(-180.0) else Degrees(0.0)))

  /**
   * These are some implicit conversions that will be handy.
   */
  import squants.space.AngleConversions.AngleConversions
  given Conversion[Angle, Double] = _.toDegrees

  given Conversion[Double, Angle] = _.degrees

  given Conversion[Angle, Latitude] = Latitude.unsafeMake(_)

  given Conversion[Angle, Longitude] = Longitude.unsafeMake(_)

  given Conversion[Latitude, Angle] = _.unwrap

  given Conversion[Longitude, Angle] = _.unwrap

  given Conversion[Longitude360, Angle] = _.unwrap

  given Conversion[Longitude360, Longitude] = denormalizeLongitude(_)

  given Conversion[Longitude, Longitude360] = normalizeLongitude(_)

  given Conversion[Long, LatitudeKey] = LatitudeKey(_)

  given Conversion[Long, LongitudeKey] = LongitudeKey(_)

  implicit object LongitudeOrder extends Order[Longitude]:
    override def order(a: Longitude, b: Longitude): Ordering =
      a.unwrap compare b.unwrap match {
        case -1 => Ordering.LT
        case 0 => Ordering.EQ
        case 1 => Ordering.GT
      }

  implicit object LatitudeOrder extends Order[Latitude]:
    override def order(a: Latitude, b: Latitude): Ordering =
      a.unwrap compare b.unwrap match {
        case -1 => Ordering.LT
        case 0 => Ordering.EQ
        case 1 => Ordering.GT
      }
