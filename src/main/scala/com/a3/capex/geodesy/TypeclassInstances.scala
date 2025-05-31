package com.a3.capex.geodesy

import squants.space.{Angle, Length}
import Coordinates.*
import neotype.unwrap

/** Contains typeclass instances for geometric types.
  *
  * These instances define how types like Angle and Length interact with typeclasses. They're kept separate from the
  * core type definitions to maintain separation of concerns and avoid implicit resolution issues.
  */
object TypeclassInstances:
  // Import the extension methods for Angle and Length
  import squants.space.AngleConversions.*
  import squants.space.LengthConversions.*

  // Type aliases for better readability
  type Lat = Latitude
  type Lon = Longitude

  // Ordering instances for basic geometric types
  given Ordering[Angle]  = Ordering.by[Angle, Double](_.toDegrees)
  given Ordering[Length] = Ordering.by[Length, Double](_.toMeters)

  /** Ordering for Longitude that handles the circular nature of longitudes. Uses a westward criteria, being careful
    * around the antimeridian.
    */
  given Ordering[Longitude] with
    def compare(a: Longitude, b: Longitude): Int =
      if a == b then 0
      else
        // Calculate the angular difference between longitudes (normalized to [-180°, 180°])
        val aDeg = a.unwrap.toDegrees // Added for clarity, though unwrap was already used
        val bDeg = b.unwrap.toDegrees // Added for clarity
        val diff = (bDeg - aDeg + 540) % 360 - 180
        if diff == 0.0 then 0    // Equivalent (e.g., 180 vs -180)
        else if diff > 0 then -1 // b is to the East of a (shorter path), so a < b
        else 1                   // b is to the West of a (shorter path, diff < 0), so a > b

  /** Ordering for Latitude, with north being greater than south.
    */
  given Ordering[Latitude] with
    def compare(a: Latitude, b: Latitude): Int =
      a.unwrap.toDegrees.compare(b.unwrap.toDegrees)

  /** Ordering for Point, primarily by latitude (south to north), secondarily by longitude (west to east, respecting
    * antimeridian).
    */
  given Ordering[ShapesCore.Point] with
    def compare(a: ShapesCore.Point, b: ShapesCore.Point): Int =
      val latCompare = summon[Ordering[Latitude]].compare(a.latitude, b.latitude)
      if latCompare != 0 then latCompare
      else summon[Ordering[Longitude]].compare(a.longitude, b.longitude)

  /** Type for combining elements through addition.
    */
  trait Addable[A]:
    def combine(a: A, b: A): A
    def empty: A

  /** Addable instance for Angle. Combines angles by adding their degree values.
    */
  given Addable[Angle] with
    def combine(a: Angle, b: Angle): Angle = (a.toDegrees + b.toDegrees).degrees
    def empty: Angle                       = 0.degrees

  /** Addable instance for Length. Combines lengths by simple addition with 0 meters as the identity element.
    */
  given Addable[Length] with
    def combine(a: Length, b: Length): Length = a + b
    def empty: Length                         = 0.meters

  /*
   * Note: We intentionally don't provide Addable instances for Longitude and Latitude
   * because adding geographic coordinates is generally not a meaningful operation.
   *
   * If you need to perform calculations with coordinates, it's better to:
   * 1. Convert to Cartesian coordinates
   * 2. Perform the calculation
   * 3. Convert back to geographic coordinates if needed
   */

  // Equivalence instances for structural equality
  given Equiv[ShapesCore.Point.PointKey] = Equiv.fromFunction(_ == _)
end TypeclassInstances
