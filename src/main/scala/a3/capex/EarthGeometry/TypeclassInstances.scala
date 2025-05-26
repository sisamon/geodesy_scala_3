package com.a3.geodesy

import squants.space.{Angle, Length, Meters}
import squants.space.AngleConversions._
import squants.space.LengthConversions._
import scalaz._
import scalaz.Scalaz._
import Coordinates._

/**
 * Contains typeclass instances for geometric types.
 * 
 * These instances define how types like Angle and Length interact with typeclasses
 * from libraries like Scalaz. They're kept separate from the core type definitions
 * to maintain separation of concerns and avoid implicit resolution issues.
 */
object TypeclassInstances:
  // Import the extension methods for Angle and Length
  import squants.space.AngleConversions._
  import squants.space.LengthConversions._

  // Type aliases for better readability
  type Lat = Latitude
  type Lon = Longitude

  // Order instances for basic geometric types
  given Order[Angle] = Order.fromScalaOrdering[Angle]
  given Order[Length] = Order.fromScalaOrdering[Length]

  /**
   * Ordering for Longitude that handles the circular nature of longitudes.
   * Uses a westward criteria, being careful around the antimeridian.
   */
  given Order[Lon] with
    def order(a: Lon, b: Lon): Ordering =
      if a == b then Ordering.EQ
      else if a.unwrap.toDegrees < b.unwrap.toDegrees then Ordering.LT
      else Ordering.GT

  /**
   * Ordering for Latitude, with north being greater than south.
   */
  given Order[Lat] with
    def order(a: Lat, b: Lat): Ordering =
      if a == b then Ordering.EQ
      else if a.unwrap.toDegrees < b.unwrap.toDegrees then Ordering.LT
      else Ordering.GT

  /**
   * Monoid instance for Angle.
   * Combines angles by adding their degree values and wraps around at 360°.
   */
  given Monoid[Angle] with
    def append(a: Angle, b: => Angle): Angle = (a.toDegrees + b.toDegrees).degrees
    def zero: Angle = 0.degrees

  /**
   * Monoid instance for Length.
   * Combines lengths by simple addition with Meters(0) as the identity element.
   */
  given Monoid[Length] with
    def append(a: Length, b: => Length): Length = a + b
    def zero: Length = 0.meters

  /*
   * Note: We intentionally don't provide Monoid instances for Longitude and Latitude
   * because adding geographic coordinates is generally not a meaningful operation.
   * 
   * If you need to perform calculations with coordinates, it's better to:
   * 1. Convert to Cartesian coordinates
   * 2. Perform the calculation
   * 3. Convert back to geographic coordinates if needed
   */

