package com.a3.capex.geodesy

/**
 * @author Luis Sisamon
 *  We are going to work on decimal degrees, we can always add UTM or
 *  other coordinates systems, but it is simpler to define the logic 
 *  using this approach.
 */

// Removed Scalaz imports
import squants.*
import squants.space.AngleConversions.*
import squants.space.{Angle, Degrees, Kilometers, Length, SquareMeters}
import neotype.*

import scala.annotation.targetName
import scala.language.implicitConversions

object Coordinates:
  val R: Length = Kilometers(6372.8) //radius in km
  val zero: Angle = Degrees(0.0)

  /**
   *  Let's take advantage of Scalas's type system to avoid swapping lat and lon
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

  /**
   * Type alias for validation results that can be either a success (true) or an error message.
   */
  type TypeValidation = Boolean | String
  
  /**
   * Represents a longitude value in degrees, constrained to valid ranges.
   * Valid ranges are either -180.0 to +180.0 (with negative for West, positive for East)
   * or 0.0 to 360.0 (with 0 at Greenwich, increasing Eastward).
   * 
   * Note: When working near the antimeridian (±180°), special care must be taken with
   * operations like min/max and comparisons, as the values wrap around.
   */
  type Longitude = Longitude.Type
  object Longitude extends Newtype[Angle]:
    /**
     * Validates that a longitude value is within acceptable ranges.
     * @param value The angle to validate
     * @return true if valid, or an error message if invalid
     */
    override inline def validate(value: Angle): TypeValidation =
      if ((value.toDegrees >= -180.0 && value.toDegrees <= 180.0) || (value.toDegrees >= 0.0 && value.toDegrees <= 360.0))
        true
      else
        "Longitude must be between -180.0 and +180.0 or between 0.0 and +360.0"

  /**
   * Represents a latitude value in degrees, constrained to -90.0 to +90.0.
   * Negative values represent the Southern hemisphere, positive values the Northern.
   * The poles are represented by -90.0 (South) and +90.0 (North).
   */
  type Latitude = Latitude.Type
  object Latitude extends Newtype[Angle]:
    /**
     * Validates that a latitude value is within -90.0 to +90.0 degrees.
     * @param value The angle to validate
     * @return true if valid, or an error message if invalid
     */
    override inline def validate(value: Angle): TypeValidation =
      if (value.toDegrees >= -90.0 && value.toDegrees <= 90.0)
        true
      else
        "Latitude must be between -90.0 and +90.0"

  /** Extension methods for Latitude values */
  extension (lat: Latitude)
    /**
     * Calculates the average between two latitudes.
     * This is useful for finding midpoints or centers.
     * 
     * @example
     * val lat1 = Latitude.unsafeMake(10.degrees)
     * val lat2 = Latitude.unsafeMake(30.degrees)
     * val avg = lat1.average(lat2)  // ~20 degrees
     */
    @targetName("lataverage")
    def average(other: Latitude): Latitude =
      // Safe because the average of two valid latitudes is always a valid latitude
      Latitude.unsafeMake(((lat.unwrap.toDegrees + other.unwrap.toDegrees) / 2).degrees)
      
    /**
     * Adds two latitude values together.
     * Note: Uses unsafeMake because the result is normalized to be within valid latitude range.
     * @deprecated Use `average` for calculating midpoints or add Angle for displacement
     */
    def +(other: Latitude): Latitude =
      // Safe to use unsafeMake here because normalize ensures the result is within valid range
      Latitude.unsafeMake(normalize(lat.unwrap + other.unwrap))
      
    /**
     * Adds an angle to a latitude.
     * Note: Uses unsafeMake because the result is normalized to be within valid latitude range.
     */
    @targetName("lataddangle")
    def +(other: Angle): Latitude =
      // Safe to use unsafeMake here because normalize ensures the result is within valid range
      Latitude.unsafeMake(normalize(lat.unwrap + other))

    /**
     * Subtracts an angle from a latitude.
     * Note: Uses unsafeMake because the result is normalized to be within valid latitude range.
     */
    def -(other: Angle): Latitude =
      // Safe to use unsafeMake here because normalize ensures the result is within valid range
      Latitude.unsafeMake(normalize(lat.unwrap - other))

    /**
     * Calculates the angular difference between two latitudes.
     * Returns an Angle representing the difference.
     */
    @targetName("latsubangle")
    def -(other: Latitude): Angle =
      normalize(lat.unwrap - other.unwrap)

    /**
     * Returns the more northern of two latitudes.
     * @param other The other latitude to compare with
     * @return The more northern latitude
     */
    @targetName("latmax")
    def max(other: Latitude): Latitude =
      if (lat.unwrap.toDegrees >= other.unwrap.toDegrees) lat else other

    /**
     * Returns the more southern of two latitudes.
     * @param other The other latitude to compare with
     * @return The more southern latitude
     */
    @targetName("latmin")
    def min(other: Latitude): Latitude =
      if (lat.unwrap.toDegrees <= other.unwrap.toDegrees) lat else other

    /**
     * Compares two latitudes for ordering.
     * @param other The other latitude to compare with
     * @return 0 if equal, 1 if this latitude is north of other, -1 if south
     */
    @targetName("latcomparee")
    def compare(other: Latitude): Int =
      val ln = lat.unwrap.toDegrees % 360
      val on = other.unwrap.toDegrees % 360
      if ln == on then 0
      else if ln > on  then 1 // This latitude is north of the other
      else -1                  // This latitude is south of the other

    // Latitudes should always remain between +/-90
    @targetName("latnormalize")
    private def normalize(value: Angle): Angle =
      val degrees = value.toDegrees
      val normalized = 
        if degrees > 0 then
          // Handle northern hemisphere
          val mod = degrees % 360
          if mod <= 90 then mod
          else if mod <= 270 then 180 - mod
          else mod - 360
        else
          // Handle southern hemisphere
          val mod = -degrees % 360
          if mod <= 90 then -mod
          else if mod <= 270 then -180 + mod
          else 360 - mod
      normalized.degrees


  /** Extension methods for Longitude values */
  extension (lon: Longitude)
    /**
     * Calculates the average between two longitudes, handling the antimeridian correctly.
     * This is useful for finding midpoints between two points on the globe.
     * 
     * @example
     * val lon1 = Longitude.unsafeMake(170.degrees)
     * val lon2 = Longitude.unsafeMake(-170.degrees)
     * val avg = lon1.average(lon2)  // 180 degrees (not 0 degrees)
     * 
     * @param other The other longitude to average with
     * @return The average longitude, correctly handling the antimeridian
     */
    @targetName("lonaverage")
    def average(other: Longitude): Longitude =
      val lon1 = lon.unwrap.toDegrees
      val lon2 = other.unwrap.toDegrees
      val (minLon, maxLon) = if lon1 < lon2 then (lon1, lon2) else (lon2, lon1)
      val avg = 
        if maxLon - minLon > 180 then
          // Handle crossing the antimeridian
          (((lon1 + 360 + lon2) / 2) % 360).degrees
        else
          ((lon1 + lon2) / 2).degrees
      Longitude.unsafeMake(avg)

    /**
     * Adds two longitude values together, with proper normalization.
     * Note: Uses unsafeMake because the result is normalized to be within -180° to +180°.
     * @deprecated Use `average` for calculating midpoints or add Angle for displacement
     */
    @targetName("lonadd")
    def +(other: Longitude): Longitude =
      // Safe to use unsafeMake here because normalize ensures the result is within valid range
      Longitude.unsafeMake(normalize(lon.unwrap + other.unwrap))

    /**
     * Adds an angle to a longitude, with proper normalization.
     * Note: Uses unsafeMake because the result is normalized to be within -180° to +180°.
     */
    @targetName("londoubleadd")
    def +(other: Angle): Longitude =
      // Safe to use unsafeMake here because normalize ensures the result is within valid range
      Longitude.unsafeMake(normalize(lon.unwrap + other))

    /**
     * Calculates the angular difference between two longitudes.
     * Returns an Angle representing the smallest difference, handling the antimeridian.
     */
    @targetName("lonsubs")
    def -(other: Longitude): Angle =
      normalize(lon.unwrap - other.unwrap)

    /**
     * Subtracts an angle from a longitude, with proper normalization.
     * Note: Uses unsafeMake because the result is normalized to be within -180° to +180°.
     */
    @targetName("londoublesub")
    def -(other: Angle): Longitude =
      // Safe to use unsafeMake here because normalize ensures the result is within valid range
      Longitude.unsafeMake(normalize(lon.unwrap - other))

    /**
     * Compares two longitudes for ordering, following a westward criteria.
     * This handles the circular nature of longitudes, being careful around the antimeridian.
     * 
     * @param other The other longitude to compare with
     * @return 0 if equal, 1 if this is west of other (and less than 180° away),
     *         -1 if this is east of other or more than 180° west
     */
    @targetName("loncompare")
    def compare(other: Longitude): Int =
      // Normalize to 0-360 range for comparison
      val ln = (lon.unwrap.toDegrees + 360.0) % 360
      val on = (other.unwrap.toDegrees + 360.0) % 360
      
      if ln == on then 0
      else if ln > on && (ln - on) < 180.0 then 1     // This is west and no more than 180° away
      else if ln > on then -1                         // This is west but more than 180° away (so actually east)
      else if (on - ln) < 180.0 then -1               // This is east and no more than 180° away
      else 1                                           // This is east but more than 180° away (so actually west)

    /**
     * Returns the more easterly of two longitudes, accounting for the circular nature 
     * and the shortest path between them.
     *
     * @param other The other longitude to compare with
     * @return The more easterly longitude
     */
    @targetName("lonmax")
    def max(other: Longitude): Longitude = {
      val diff = (other.unwrap.toDegrees - lon.unwrap.toDegrees + 540) % 360 - 180
      if (diff > 0 && diff <= 180) other else lon
    }

    /**
     * Returns the more westerly of two longitudes, accounting for the circular nature 
     * and the shortest path between them.
     *
     * @param other The other longitude to compare with
     * @return The more westerly longitude
     */
    @targetName("lonmin")
    def min(other: Longitude): Longitude = {
      val diff = (other.unwrap.toDegrees - lon.unwrap.toDegrees + 540) % 360 - 180
      if (diff < 0 || diff > 180) other else lon
    }
    
    /**
     * Normalizes a longitude angle to the range [-180°, +180°].
     * This handles wrapping around the antimeridian correctly.
     * 
     * @param value The angle to normalize
     * @return The normalized angle in the range [-180°, +180°]
     */
    @targetName("lonnormalize")
    private def normalize(value: Angle): Angle =
      ((value.toDegrees + 540) % 360 - 180).degrees

    /**
     * Ensures the longitude is in the standard range of -180° to +180°.
     * This is useful after operations that might produce values outside this range.
     * 
     * @return A new Longitude normalized to [-180°, +180°]
     * @note Uses unsafeMake because we're just adjusting the range, not changing the actual value
     */
    def denormalize(): Longitude =
      // Safe to use unsafeMake here because we're just adjusting the range
      Longitude.unsafeMake(lon.unwrap + (if (lon.unwrap > Degrees(180.0)) Degrees(-360.0) else Degrees(0.0)))

  /**
   * Opaque type representing a longitude value optimized for use as a map key.
   * This is a lossy representation that provides fast hashing and comparison.
   * The value represents degrees * 1e6 (microdegrees) as a Long.
   */
  opaque type LongitudeKey = Long
  object LongitudeKey:
    /** Create a LongitudeKey from a raw Long value (microdegrees) */
    def apply(value: Long): LongitudeKey = value
    
    /** Create a LongitudeKey from an Angle */
    def fromAngle(angle: Angle): LongitudeKey = 
      (angle.toDegrees * 1e6).toLong
      
    /** Convert back to an Angle (approximate due to lossy conversion) */
    def toAngle(key: LongitudeKey): Angle = 
      (key.toDouble / 1e6).degrees
    
    extension (key: LongitudeKey)
      /** Convert to a human-readable string */
      def toDegrees: Double = key.toDouble / 1e6
      
      /** String representation in degrees */
      def toString: String = s"$toDegrees"

  /**
   * Opaque type representing a latitude value optimized for use as a map key.
   * This is a lossy representation that provides fast hashing and comparison.
   * The value represents degrees * 1e6 (microdegrees) as a Long.
   */
  opaque type LatitudeKey = Long
  object LatitudeKey:
    /** Create a LatitudeKey from a raw Long value (microdegrees) */
    def apply(value: Long): LatitudeKey = value
    
    /** Create a LatitudeKey from an Angle */
    def fromAngle(angle: Angle): LatitudeKey = 
      (angle.toDegrees * 1e6).toLong
      
    /** Convert back to an Angle (approximate due to lossy conversion) */
    def toAngle(key: LatitudeKey): Angle = 
      (key.toDouble / 1e6).degrees
    
    extension (key: LatitudeKey)
      /** Convert to a human-readable string */
      def toDegrees: Double = key.toDouble / 1e6
      
      /** String representation in degrees */
      def toString: String = s"$toDegrees"


object CoordinatesImplicits:
  import com.a3.capex.geodesy.Coordinates.{Latitude, Longitude, LongitudeKey, LatitudeKey, zero} // Longitude360,
  import squants.space.{Angle, Degrees, Kilometers, SquareMeters, Length}
  import squants.space.AngleConversions.AngleConversions
  import neotype._

  /**
   * These are some implicit conversions that will be handy.
   */
  given Conversion[Angle, Double] = _.toDegrees
  given Conversion[Double, Angle] = _.degrees
  given Conversion[Angle, Latitude] = Latitude.unsafeMake(_)
  given Conversion[Angle, Longitude] = Longitude.unsafeMake(_)
  given Conversion[Latitude, Angle] = _.unwrap
  given Conversion[Longitude, Angle] = _.unwrap
  given Conversion[Long, LatitudeKey] = LatitudeKey(_)
  given Conversion[Long, LongitudeKey] = LongitudeKey(_)
  given Conversion[Latitude, Double] = _.unwrap.toDegrees
  given Conversion[Longitude, Double] = _.unwrap.toDegrees

  // Typeclass instances have been moved to TypeclassInstances.scala
  // Import them with: import com.a3.geodesy.TypeclassInstances._
  
  /*
   * Note: We intentionally don't provide Monoid instances for Longitude and Latitude
   * because adding geographic coordinates is generally not a meaningful operation.
   * 
   * If you need to perform calculations with coordinates, it's better to:
   * 1. Convert to Cartesian coordinates
   * 2. Perform the calculation
   * 3. Convert back to geographic coordinates if needed
   */
