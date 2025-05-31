package com.a3.capex.geodesy

import munit.FunSuite
import com.a3.capex.geodesy.Coordinates.* // Import types and methods
import squants.space.{Angle, Degrees}     // Import Squants types
import neotype.unwrap                     // Import unwrap extension method for Neotype

class CoordinatesSuite extends munit.FunSuite {

  // --- Latitude Tests ---
  test("Latitude.validate should accept valid latitudes") {
    assertEquals(Latitude.validate(Degrees(0.0)), true)
    assertEquals(Latitude.validate(Degrees(90.0)), true)
    assertEquals(Latitude.validate(Degrees(-90.0)), true)
    assertEquals(Latitude.validate(Degrees(45.0)), true)
    assertEquals(Latitude.validate(Degrees(-30.0)), true)
  }

  test("Latitude.validate should reject invalid latitudes") {
    assertEquals(Latitude.validate(Degrees(90.1)), "Latitude must be between -90.0 and +90.0")
    assertEquals(Latitude.validate(Degrees(-90.1)), "Latitude must be between -90.0 and +90.0")
    assertEquals(Latitude.validate(Degrees(180.0)), "Latitude must be between -90.0 and +90.0")
  }

  test("Latitude.make should create valid latitudes from runtime values") {
    val latEither = Latitude.make(Degrees(30.0))
    assert(latEither.isRight, "Latitude.make should succeed for valid input")
    assertEquals(latEither.getOrElse(fail("Expected Right")).unwrap, Degrees(30.0))
  }

  test("Latitude.make should fail for invalid runtime latitudes with correct message") {
    val latEither = Latitude.make(Degrees(95.0))
    assert(latEither.isLeft, "Latitude.make should fail for invalid input")
    assertEquals(latEither.left.getOrElse(fail("Expected Left")), "Latitude must be between -90.0 and +90.0")
  }

  test("Latitude.unsafeMake should create latitudes even if invalid according to validate") {
    val lat = Latitude.unsafeMake(Degrees(95.0)) // Should not throw
    assertEquals(lat.unwrap, Degrees(95.0))
  }

  // --- Longitude Tests ---
  test("Longitude.validate should accept valid longitudes") {
    assertEquals(Longitude.validate(Degrees(0.0)), true)
    assertEquals(Longitude.validate(Degrees(180.0)), true)
    assertEquals(Longitude.validate(Degrees(-180.0)), true)
    // Assuming 0-360 is also a valid input range for the underlying Angle before normalization by Newtype
    assertEquals(Longitude.validate(Degrees(360.0)), true)
    assertEquals(Longitude.validate(Degrees(90.0)), true)
  }

  test("Longitude.validate should reject invalid longitudes") {
    assertEquals(
      Longitude.validate(Degrees(360.1)),
      "Longitude must be between -180.0 and +180.0 or between 0.0 and +360.0"
    )
    assertEquals(
      Longitude.validate(Degrees(-180.1)),
      "Longitude must be between -180.0 and +180.0 or between 0.0 and +360.0"
    )
  }

  test("Longitude.make should create valid longitudes from runtime values") {
    val lonEither = Longitude.make(Degrees(120.0))
    assert(lonEither.isRight, "Longitude.make should succeed for valid input")
    assertEquals(lonEither.getOrElse(fail("Expected Right")).unwrap, Degrees(120.0))
  }

  test("Longitude.make should fail for invalid runtime longitudes with correct message") {
    val lonEither = Longitude.make(Degrees(-190.0))
    assert(lonEither.isLeft, "Longitude.make should fail for invalid input")
    assertEquals(
      lonEither.left.getOrElse(fail("Expected Left")),
      "Longitude must be between -180.0 and +180.0 or between 0.0 and +360.0"
    )
  }

  test("Longitude.unsafeMake should create longitudes even if invalid according to validate") {
    val lon = Longitude.unsafeMake(Degrees(-190.0)) // Should not throw
    assertEquals(lon.unwrap, Degrees(-190.0))
  }

  // --- Haversine Tests ---
  test("haversine function should return correct values") {
    assertEqualsDouble(Coordinates.haversine(0.0), 0.0, 1e-9)
    assertEqualsDouble(Coordinates.haversine(Math.PI), 1.0, 1e-9)
    assertEqualsDouble(Coordinates.haversine(Math.PI / 2.0), 0.5, 1e-9)
    // Test with a random angle, e.g., PI/3 (60 degrees)
    // haversine(PI/3) = sin(PI/6)^2 = (0.5)^2 = 0.25
    assertEqualsDouble(Coordinates.haversine(Math.PI / 3.0), 0.25, 1e-9)
  }

  // --- Latitude Extension Methods ---

  // Note: Using unsafeMake for brevity in test setup for Latitude instances.
  // Validation and .make are tested separately.

  test("Latitude.average should correctly average two latitudes") {
    assertEquals(
      Latitude.unsafeMake(Degrees(10.0)).average(Latitude.unsafeMake(Degrees(30.0))),
      Latitude.unsafeMake(Degrees(20.0))
    )
    assertEquals(
      Latitude.unsafeMake(Degrees(-10.0)).average(Latitude.unsafeMake(Degrees(-30.0))),
      Latitude.unsafeMake(Degrees(-20.0))
    )
    assertEquals(
      Latitude.unsafeMake(Degrees(10.0)).average(Latitude.unsafeMake(Degrees(-30.0))),
      Latitude.unsafeMake(Degrees(-10.0))
    )
    assertEquals(
      Latitude.unsafeMake(Degrees(20.0)).average(Latitude.unsafeMake(Degrees(0.0))),
      Latitude.unsafeMake(Degrees(10.0))
    )
    assertEquals(
      Latitude.unsafeMake(Degrees(45.0)).average(Latitude.unsafeMake(Degrees(45.0))),
      Latitude.unsafeMake(Degrees(45.0))
    )
  }

  test("Latitude + Angle should add and normalize correctly") {
    val lat80        = Latitude.unsafeMake(Degrees(80.0))
    val lat_minus_80 = Latitude.unsafeMake(Degrees(-80.0))
    val lat0         = Latitude.unsafeMake(Degrees(0.0))

    assertEquals(lat80 + Degrees(5.0), Latitude.unsafeMake(Degrees(85.0)), "Stays within bounds")
    // 80 + 15 = 95. Normalized: 180 - 95 = 85
    assertEquals(lat80 + Degrees(15.0), Latitude.unsafeMake(Degrees(85.0)), "Goes over North Pole")
    // -80 + (-15) = -95. Normalized: -180 - (-95) = -85
    assertEquals(lat_minus_80 + Degrees(-15.0), Latitude.unsafeMake(Degrees(-85.0)), "Goes over South Pole")
    // 0 + 100 = 100. Normalized: 180 - 100 = 80
    assertEquals(lat0 + Degrees(100.0), Latitude.unsafeMake(Degrees(80.0)), "From zero over North Pole")
    // 0 + (-100) = -100. Normalized: -180 - (-100) = -80
    assertEquals(lat0 + Degrees(-100.0), Latitude.unsafeMake(Degrees(-80.0)), "From zero over South Pole")
    // 80 + 290 = 370. cycles = floor((370+90)/360) = 1. reduced = 370 - 360 = 10. Normalized: 10
    assertEquals(lat80 + Degrees(290.0), Latitude.unsafeMake(Degrees(10.0)), "Large angle wrapping")
    // 80 + 280 = 360. cycles = floor((360+90)/360) = 1. reduced = 360 - 360 = 0. Normalized: 0
    assertEquals(lat80 + Degrees(280.0), Latitude.unsafeMake(Degrees(0.0)), "Large angle wrapping to 0")
  }

  test("Latitude - Angle should subtract and normalize correctly") {
    val lat80        = Latitude.unsafeMake(Degrees(80.0))
    val lat_minus_80 = Latitude.unsafeMake(Degrees(-80.0))

    assertEquals(lat80 - Degrees(5.0), Latitude.unsafeMake(Degrees(75.0)), "Stays within bounds")
    // -80 - 15 = -95. Normalized: -180 - (-95) = -85
    assertEquals(lat_minus_80 - Degrees(15.0), Latitude.unsafeMake(Degrees(-85.0)), "Goes over South Pole")
    // 80 - (-15) = 95. Normalized: 180 - 95 = 85
    assertEquals(lat80 - Degrees(-15.0), Latitude.unsafeMake(Degrees(85.0)), "Subtract negative, goes over North Pole")
  }

  test("Latitude.compare should correctly compare two latitudes") {
    val lat10        = Latitude.unsafeMake(Degrees(10.0))
    val lat20        = Latitude.unsafeMake(Degrees(20.0))
    val lat_minus_10 = Latitude.unsafeMake(Degrees(-10.0))
    val lat_minus_20 = Latitude.unsafeMake(Degrees(-20.0))

    assertEquals(lat10.compare(lat20), -1, "10 < 20")
    assertEquals(lat20.compare(lat10), 1, "20 > 10")
    assertEquals(lat10.compare(Latitude.unsafeMake(Degrees(10.0))), 0, "10 == 10")
    assertEquals(lat_minus_10.compare(lat_minus_20), 1, "-10 > -20")
    assertEquals(lat_minus_20.compare(lat_minus_10), -1, "-20 < -10")
    assertEquals(lat10.compare(lat_minus_10), 1, "10 > -10")
  }

  // --- Longitude Extension Methods ---

  // Note: Using unsafeMake for brevity in test setup for Longitude instances.
  // Validation and .make are tested separately.

  test("Longitude.average should correctly average two longitudes, handling antimeridian") {
    assertEquals(
      Longitude.unsafeMake(Degrees(10.0)).average(Longitude.unsafeMake(Degrees(30.0))),
      Longitude.unsafeMake(Degrees(20.0))
    )
    assertEquals(
      Longitude.unsafeMake(Degrees(-10.0)).average(Longitude.unsafeMake(Degrees(-30.0))),
      Longitude.unsafeMake(Degrees(-20.0))
    )
    assertEquals(
      Longitude.unsafeMake(Degrees(-10.0)).average(Longitude.unsafeMake(Degrees(10.0))),
      Longitude.unsafeMake(Degrees(0.0))
    )
    // Average of 170 and 190 (which is -170) is 180.
    assertEquals(
      Longitude.unsafeMake(Degrees(170.0)).average(Longitude.unsafeMake(Degrees(-170.0))),
      Longitude.unsafeMake(Degrees(180.0))
    )
    // Average of 350 (which is -10) and 10 is 0.
    assertEquals(
      Longitude.unsafeMake(Degrees(350.0)).average(Longitude.unsafeMake(Degrees(10.0))),
      Longitude.unsafeMake(Degrees(0.0))
    )
    assertEquals(
      Longitude.unsafeMake(Degrees(10.0)).average(Longitude.unsafeMake(Degrees(10.0))),
      Longitude.unsafeMake(Degrees(10.0))
    )
  }

  test("Longitude + Angle should add and normalize to [-180, 180) correctly") {
    val lon170        = Longitude.unsafeMake(Degrees(170.0))
    val lon_minus_170 = Longitude.unsafeMake(Degrees(-170.0))
    val lon10         = Longitude.unsafeMake(Degrees(10.0))

    assertEquals(lon170 + Degrees(5.0), Longitude.unsafeMake(Degrees(175.0)), "Stays within bounds")
    assertEquals(lon170 + Degrees(20.0), Longitude.unsafeMake(Degrees(-170.0)), "Wraps eastward over antimeridian")
    assertEquals(
      lon_minus_170 + Degrees(-20.0),
      Longitude.unsafeMake(Degrees(170.0)),
      "Wraps westward over antimeridian"
    )
    assertEquals(
      lon10 + Degrees(370.0),
      Longitude.unsafeMake(Degrees(20.0)),
      "Large angle wrapping: 10 + (370 % 360) = 10 + 10 = 20"
    )
    assertEquals(
      lon10 + Degrees(-370.0),
      Longitude.unsafeMake(Degrees(0.0)),
      "Large negative angle wrapping: 10 + (-370 % 360) = 10 - 10 = 0"
    )
  }

  test("Longitude - Angle should subtract and normalize to [-180, 180) correctly") {
    val lon170        = Longitude.unsafeMake(Degrees(170.0))
    val lon_minus_170 = Longitude.unsafeMake(Degrees(-170.0))

    assertEquals(lon_minus_170 - Degrees(20.0), Longitude.unsafeMake(Degrees(170.0)), "-170 - 20 = -190 => 170")
    assertEquals(lon170 - Degrees(-20.0), Longitude.unsafeMake(Degrees(-170.0)), "170 - (-20) = 190 => -170")
  }

  test("Longitude.compare should correctly compare two longitudes (west to east)") {
    val lon10         = Longitude.unsafeMake(Degrees(10.0))
    val lon20         = Longitude.unsafeMake(Degrees(20.0))
    val lon170        = Longitude.unsafeMake(Degrees(170.0))
    val lon_minus_170 = Longitude.unsafeMake(Degrees(-170.0)) // effectively 190

    assertEquals(lon10.compare(lon20), -1, "10E is west of 20E")
    assertEquals(lon20.compare(lon10), 1, "20E is east of 10E")
    assertEquals(lon10.compare(Longitude.unsafeMake(Degrees(10.0))), 0, "10E == 10E")
    // 170E is west of -170E (190E)
    assertEquals(lon170.compare(lon_minus_170), -1, "170E is west of -170E (190E)")
    // -170E (190E) is east of 170E
    assertEquals(lon_minus_170.compare(lon170), 1, "-170E (190E) is east of 170E")
  }

  test("Longitude.min should return the more westerly longitude") {
    val lon10         = Longitude.unsafeMake(Degrees(10.0))
    val lon20         = Longitude.unsafeMake(Degrees(20.0))
    val lon170        = Longitude.unsafeMake(Degrees(170.0))
    val lon_minus_170 = Longitude.unsafeMake(Degrees(-170.0))

    assertEquals(lon10.min(lon20), lon10)
    assertEquals(lon170.min(lon_minus_170), lon170)
  }

  test("Longitude.max should return the more easterly longitude") {
    val lon10         = Longitude.unsafeMake(Degrees(10.0))
    val lon20         = Longitude.unsafeMake(Degrees(20.0))
    val lon170        = Longitude.unsafeMake(Degrees(170.0))
    val lon_minus_170 = Longitude.unsafeMake(Degrees(-170.0))

    assertEquals(lon10.max(lon20), lon20)
    assertEquals(lon170.max(lon_minus_170), lon_minus_170)
  }

  test("Longitude.denormalize should convert [0,360) style to [-180,180)") {
    // denormalize is: lon.unwrap + (if (lon.unwrap > Degrees(180.0)) Degrees(-360.0) else Degrees(0.0))
    // This assumes the input `lon.unwrap` might be from a 0-360 context.
    // If `Longitude.unsafeMake` already normalizes to [-180,180), then `lon.unwrap` will be in that range.
    // Let's test its defined behavior based on the formula.
    // To properly test denormalize, we need to simulate an underlying Angle that is > 180.
    // We can't directly do that with Longitude.unsafeMake if it normalizes.
    // However, the `unwrap` gives the Angle, so we can construct an Angle outside the range.

    // Test case 1: Angle is 270 degrees. Denormalize should make it -90.
    val lon270 = Longitude.unsafeMake(Degrees(270.0)) // unsafeMake might normalize this to -90 already.
    // If so, lon270.unwrap is -90. Then denormalize does nothing. This is fine.
    // Let's check Coordinates.scala: Longitude extends Newtype[Angle].
    // Newtype itself doesn't normalize on unsafeMake. So Degrees(270.0) is preserved.
    assertEquals(lon270.denormalize().unwrap.toDegrees, -90.0, 1e-9)

    // Test case 2: Angle is 170 degrees. Denormalize should do nothing.
    val lon170 = Longitude.unsafeMake(Degrees(170.0))
    assertEquals(lon170.denormalize().unwrap.toDegrees, 170.0, 1e-9)

    // Test case 3: Angle is -90 degrees. Denormalize should do nothing.
    val lon_minus_90 = Longitude.unsafeMake(Degrees(-90.0))
    assertEquals(lon_minus_90.denormalize().unwrap.toDegrees, -90.0, 1e-9)

    // Test case 4: Angle is 180.0 degrees. Denormalize should do nothing.
    val lon180 = Longitude.unsafeMake(Degrees(180.0))
    assertEquals(lon180.denormalize().unwrap.toDegrees, 180.0, 1e-9)

    // Test case 5: Angle is 180.1 degrees. Denormalize should make it approx -179.9.
    val lon180_1 = Longitude.unsafeMake(Degrees(180.1))
    assertEquals(lon180_1.denormalize().unwrap.toDegrees, 180.1 - 360.0, 1e-9)
  }

  // --- LatitudeKey Tests ---

  test("LatitudeKey.apply should create a key and preserve the Long value") {
    val rawValue = 1234567890L
    val latKey   = LatitudeKey(rawValue)
    // We can't directly access the underlying Long due to opaque type, but we can test conversions
    assertEquals(latKey.toDegrees, rawValue.toDouble / 1e6, 1e-9)
  }

  test("LatitudeKey.fromAngle should convert Angle to microdegrees Long") {
    assertEquals(LatitudeKey.fromAngle(Degrees(0.0)), LatitudeKey(0L))
    assertEquals(LatitudeKey.fromAngle(Degrees(45.0)), LatitudeKey(45000000L))
    assertEquals(LatitudeKey.fromAngle(Degrees(-30.0)), LatitudeKey(-30000000L))
    assertEquals(LatitudeKey.fromAngle(Degrees(12.345678)), LatitudeKey(12345678L))  // Truncation expected by toLong
    assertEquals(LatitudeKey.fromAngle(Degrees(12.3456789)), LatitudeKey(12345678L)) // Truncation of .9
    assertEquals(LatitudeKey.fromAngle(Degrees(-12.345678)), LatitudeKey(-12345678L))
  }

  test("LatitudeKey.toAngle should convert key back to Angle (approximate)") {
    val originalAngle = Degrees(45.123456)
    val latKey        = LatitudeKey.fromAngle(originalAngle)
    assertEquals(LatitudeKey.toAngle(latKey).toDegrees, originalAngle.toDegrees, 1e-9)

    val latKeyFromRaw = LatitudeKey(12345678L)
    assertEquals(LatitudeKey.toAngle(latKeyFromRaw).toDegrees, 12.345678, 1e-9)
  }

  test("LatitudeKey.toDegrees extension should convert key to Double degrees") {
    assertEquals(LatitudeKey(45000000L).toDegrees, 45.0, 1e-9)
    assertEquals(LatitudeKey(12345678L).toDegrees, 12.345678, 1e-9)
    assertEquals(LatitudeKey(-30000000L).toDegrees, -30.0, 1e-9)
  }

  test("LatitudeKey.asString extension should produce correct string representation") {
    assertEquals(LatitudeKey(45000000L).asString, "45.0")
    assertEquals(LatitudeKey(12345678L).asString, "12.345678")
    assertEquals(LatitudeKey(-30000000L).asString, "-30.0")
    assertEquals(LatitudeKey(0L).asString, "0.0")
  }

  test("LatitudeKey should work correctly as a Map key") {
    val key1     = LatitudeKey.fromAngle(Degrees(10.0))
    val key2     = LatitudeKey.fromAngle(Degrees(20.0))
    val key1_dup = LatitudeKey(10000000L)

    val map = Map(key1 -> "Value1", key2 -> "Value2")

    assertEquals(map.get(key1), Some("Value1"))
    assertEquals(map.get(key2), Some("Value2"))
    assertEquals(map.get(key1_dup), Some("Value1")) // Test equality
    assertEquals(map.get(LatitudeKey.fromAngle(Degrees(30.0))), None)
  }

  // --- LongitudeKey Tests ---

  test("LongitudeKey.apply should create a key and preserve the Long value") {
    val rawValue = 9876543210L
    val lonKey   = LongitudeKey(rawValue)
    assertEquals(lonKey.toDegrees, rawValue.toDouble / 1e6, 1e-9)
  }

  test("LongitudeKey.fromAngle should convert Angle to microdegrees Long") {
    assertEquals(LongitudeKey.fromAngle(Degrees(0.0)), LongitudeKey(0L))
    assertEquals(LongitudeKey.fromAngle(Degrees(170.0)), LongitudeKey(170000000L))
    assertEquals(LongitudeKey.fromAngle(Degrees(-150.0)), LongitudeKey(-150000000L))
    assertEquals(LongitudeKey.fromAngle(Degrees(88.765432)), LongitudeKey(88765432L))
    assertEquals(LongitudeKey.fromAngle(Degrees(88.7654321)), LongitudeKey(88765432L))
    assertEquals(LongitudeKey.fromAngle(Degrees(-88.765432)), LongitudeKey(-88765432L))
  }

  test("LongitudeKey.toAngle should convert key back to Angle (approximate)") {
    val originalAngle = Degrees(120.987654)
    val lonKey        = LongitudeKey.fromAngle(originalAngle)
    assertEquals(LongitudeKey.toAngle(lonKey).toDegrees, originalAngle.toDegrees, 1e-9)

    val lonKeyFromRaw = LongitudeKey(88765432L)
    assertEquals(LongitudeKey.toAngle(lonKeyFromRaw).toDegrees, 88.765432, 1e-9)
  }

  test("LongitudeKey.toDegrees extension should convert key to Double degrees") {
    assertEquals(LongitudeKey(170000000L).toDegrees, 170.0, 1e-9)
    assertEquals(LongitudeKey(88765432L).toDegrees, 88.765432, 1e-9)
    assertEquals(LongitudeKey(-150000000L).toDegrees, -150.0, 1e-9)
  }

  test("LongitudeKey.asString extension should produce correct string representation") {
    assertEquals(LongitudeKey(170000000L).asString, "170.0")
    assertEquals(LongitudeKey(88765432L).asString, "88.765432")
    assertEquals(LongitudeKey(-150000000L).asString, "-150.0")
    assertEquals(LongitudeKey(0L).asString, "0.0")
  }

  test("LongitudeKey should work correctly as a Map key") {
    val key1     = LongitudeKey.fromAngle(Degrees(90.0))
    val key2     = LongitudeKey.fromAngle(Degrees(-90.0))
    val key1_dup = LongitudeKey(90000000L)

    val map = Map(key1 -> "East90", key2 -> "West90")

    assertEquals(map.get(key1), Some("East90"))
    assertEquals(map.get(key2), Some("West90"))
    assertEquals(map.get(key1_dup), Some("East90"))
    assertEquals(map.get(LongitudeKey.fromAngle(Degrees(180.0))), None)
  }

  // TODO: Add tests for implicits in CoordinatesImplicits (if deemed necessary to test directly)
}
