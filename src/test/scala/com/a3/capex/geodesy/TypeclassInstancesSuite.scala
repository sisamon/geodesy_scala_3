package com.a3.capex.geodesy

import munit.FunSuite
import squants.space.{Angle, Length}
import squants.space.AngleConversions._
import squants.space.LengthConversions._
import com.a3.capex.geodesy.Coordinates.{Latitude, Longitude}
import com.a3.capex.geodesy.TypeclassInstances._ // Import the givens
import com.a3.capex.geodesy.TypeclassInstances.{given_Ordering_Latitude, given_Ordering_Longitude}

class TypeclassInstancesSuite extends FunSuite {

  // --- Ordering Tests --- 

  test("Ordering[Angle] should compare angles correctly") {
    val angleOrd = summon[Ordering[Angle]]
    assert(angleOrd.lt(0.degrees, 10.degrees), "0 deg < 10 deg")
    assert(angleOrd.gt(10.degrees, 0.degrees), "10 deg > 0 deg")
    assert(angleOrd.equiv(10.degrees, 10.degrees), "10 deg == 10 deg")
    assert(angleOrd.lt(-10.degrees, 10.degrees), "-10 deg < 10 deg")
    assert(angleOrd.gt(10.degrees, -10.degrees), "10 deg > -10 deg")
  }

  test("Ordering[Length] should compare lengths correctly") {
    val lengthOrd = summon[Ordering[Length]]
    assert(lengthOrd.lt(0.meters, 10.meters), "0m < 10m")
    assert(lengthOrd.gt(10.meters, 0.meters), "10m > 0m")
    assert(lengthOrd.equiv(10.meters, 10.meters), "10m == 10m")
    assert(lengthOrd.lt(5.kilometers, 10.kilometers), "5km < 10km")
  }

  test("Ordering[Latitude] should compare latitudes correctly (North > South)") {
    val latOrd = summon[Ordering[Latitude]]
    val lat10N = Latitude.unsafeMake(10.degrees)
    val lat20N = Latitude.unsafeMake(20.degrees)
    val lat10S = Latitude.unsafeMake(-10.degrees)
    val lat20S = Latitude.unsafeMake(-20.degrees)

    assert(latOrd.lt(lat10N, lat20N), "10N < 20N")
    assert(latOrd.gt(lat20N, lat10N), "20N > 10N")
    assert(latOrd.lt(lat20S, lat10S), "20S < 10S") // -20 is south of -10
    assert(latOrd.gt(lat10S, lat20S), "10S > 20S")
    assert(latOrd.lt(lat10S, lat10N), "10S < 10N")
    assert(latOrd.gt(lat10N, lat10S), "10N > 10S")
    assert(latOrd.equiv(lat10N, Latitude.unsafeMake(10.degrees)), "10N == 10N")
  }

  test("Ordering[Longitude] should compare longitudes correctly (handles antimeridian)") {
    val lonOrd = summon[Ordering[Longitude]]
    
    val lon10E = Longitude.unsafeMake(10.degrees)
    val lon20E = Longitude.unsafeMake(20.degrees)
    val lon10W = Longitude.unsafeMake(-10.degrees)
    val lon20W = Longitude.unsafeMake(-20.degrees)
    val lon170E = Longitude.unsafeMake(170.degrees)
    val lon170W = Longitude.unsafeMake(-170.degrees) // same as 190 degrees East
    val lon0 = Longitude.unsafeMake(0.degrees)
    val lon180 = Longitude.unsafeMake(180.degrees)
    val lonMinus180 = Longitude.unsafeMake(-180.degrees) // same as 180 degrees

    // Standard comparisons
    assert(lonOrd.lt(lon10E, lon20E), "10E < 20E")
    assert(lonOrd.gt(lon20E, lon10E), "20E > 10E")
    assert(lonOrd.lt(lon20W, lon10W), "20W < 10W (-20 < -10)")
    assert(lonOrd.gt(lon10W, lon20W), "10W > 20W (-10 > -20)")
    assert(lonOrd.equiv(lon10E, Longitude.unsafeMake(10.degrees)), "10E == 10E")

    // Antimeridian crossing
    assert(lonOrd.lt(lon170E, lon170W), "170E < -170W (170E is west of 190E)")
    assert(lonOrd.gt(lon170W, lon170E), "-170W > 170E (190E is east of 170E)")
    
    // Comparisons with 0/180
    assert(lonOrd.lt(lon10W, lon10E), "-10W < 10E")
    assert(lonOrd.lt(lon0, lon10E), "0 < 10E")
    assert(lonOrd.gt(lon0, lon10W), "0 > -10W")
    assert(lonOrd.lt(lon170E, lon180), "170E < 180")
    assert(lonOrd.lt(lon170E, lonMinus180), "170E < -180 (which is 180)")
    assert(lonOrd.equiv(lon180, lonMinus180), "180 == -180")
    assert(lonOrd.gt(lon10W, lon180), "-10W > 180 (180 is west of -10W via shorter path)") // -10W is 350. 180 vs 350. (350-180+540)%360-180 = (170+540)%360-180 = 710%360-180 = 350-180 = 170. No, this is b-a. (180 - (-10) + 540)%360-180 = (190+540)%360-180 = 730%360-180 = 10-180 = -170. False. So 1. -10W > 180. Correct.
  }

  // --- Addable Tests --- 

  test("Addable[Angle] should combine angles and provide empty") {
    val angleAddable = summon[Addable[Angle]]
    assertEquals(angleAddable.combine(10.degrees, 20.degrees), 30.degrees, "10deg + 20deg = 30deg")
    assertEquals(angleAddable.combine(10.degrees, -5.degrees), 5.degrees, "10deg + (-5deg) = 5deg")
    assertEquals(angleAddable.empty, 0.degrees, "empty Angle is 0deg")
  }

  test("Addable[Length] should combine lengths and provide empty (assuming standard addition)") {
    val lengthAddable = summon[Addable[Length]]
    assertEquals(lengthAddable.combine(10.meters, 20.meters), 30.meters, "10m + 20m = 30m")
    assertEquals(lengthAddable.combine(100.centimeters, 1.meters), 2.meters, "100cm + 1m = 2m")
    assertEquals(lengthAddable.empty, 0.meters, "empty Length is 0m")
  }
}
