package com.a3.capex.geodesy

import munit.FunSuite
import com.a3.capex.geodesy.ShapesCore.{Point, Box}
import com.a3.capex.geodesy.Coordinates.{Latitude, Longitude}
import squants.space.AngleConversions._ // For .degrees
import squants.space.AreaConversions._ // For .squareMeters etc.

class ShapesCoreSuite extends FunSuite {

  // --- Point Tests ---
  test("Point creation and basic properties") {
    val lat = Latitude.unsafeMake(45.degrees)
    val lon = Longitude.unsafeMake(90.degrees)
    val point = Point(lat, lon)

    assertEquals(point.latitude, lat, "Point latitude should match input")
    assertEquals(point.longitude, lon, "Point longitude should match input")
  }

  test("Point.isSorted should correctly compare points") {
    val p1_lat10_lon20 = Point(Latitude.unsafeMake(10.degrees), Longitude.unsafeMake(20.degrees))
    val p2_lat5_lon30  = Point(Latitude.unsafeMake(5.degrees),  Longitude.unsafeMake(30.degrees)) // lat1 > lat2
    val p3_lat10_lon15 = Point(Latitude.unsafeMake(10.degrees), Longitude.unsafeMake(15.degrees)) // lat1 == lat3, lon1 > lon3
    val p4_lat10_lon20 = Point(Latitude.unsafeMake(10.degrees), Longitude.unsafeMake(20.degrees)) // identical to p1

    // Test latitude primary sort
    assert(p2_lat5_lon30.isSorted(p1_lat10_lon20), "(5,30) should be sorted before (10,20) due to latitude")
    assert(!p1_lat10_lon20.isSorted(p2_lat5_lon30), "(10,20) should not be sorted before (5,30) due to latitude")

    // Test longitude secondary sort (when latitudes are equal)
    assert(p3_lat10_lon15.isSorted(p1_lat10_lon20), "(10,15) should be sorted before (10,20) due to longitude")
    assert(!p1_lat10_lon20.isSorted(p3_lat10_lon15), "(10,20) should not be sorted before (10,15) due to longitude")

    // Test identical points
    // isSorted implies strictly less than for one of the components if not equal for the other.
    // So, p1.isSorted(p1) should be false because neither latitude nor longitude is strictly less.
    assert(!p1_lat10_lon20.isSorted(p4_lat10_lon20), "A point should not be sorted before an identical point")
    assert(!p4_lat10_lon20.isSorted(p1_lat10_lon20), "An identical point should not be sorted before the original point")
  }

  test("Point.orientation should determine orientation of three points") {
    val p00 = Point(Latitude.unsafeMake(0.degrees), Longitude.unsafeMake(0.degrees))
    val p11 = Point(Latitude.unsafeMake(1.degrees), Longitude.unsafeMake(1.degrees))
    val p10 = Point(Latitude.unsafeMake(1.degrees), Longitude.unsafeMake(0.degrees))
    val p01 = Point(Latitude.unsafeMake(0.degrees), Longitude.unsafeMake(1.degrees))
    val p22 = Point(Latitude.unsafeMake(2.degrees), Longitude.unsafeMake(2.degrees))

    // Collinear cases
    assertEquals(Point.orientation(p00, p11, p22), 0, "Points (0,0)-(1,1)-(2,2) should be collinear")
    assertEquals(Point.orientation(p00, p10, Point(Latitude.unsafeMake(2.degrees), Longitude.unsafeMake(0.degrees))), 0, "Horizontal points should be collinear")
    assertEquals(Point.orientation(p00, p01, Point(Latitude.unsafeMake(0.degrees), Longitude.unsafeMake(2.degrees))), 0, "Vertical points should be collinear")

    // Counter-clockwise case: (0,0) -> (1,0) -> (1,1) (Origin -> North -> East relative to North vector)
    assertEquals(Point.orientation(p00, p10, p11), 2, "(0,0)-(1,0)-(1,1) should be counter-clockwise")

    // Clockwise case: (0,0) -> (0,1) -> (1,1) (Origin -> East -> North relative to East vector)
    assertEquals(Point.orientation(p00, p01, p11), 1, "(0,0)-(0,1)-(1,1) should be clockwise")
  }

  test("Point.onSegment should check if a point lies on a segment") {
    val p1_00 = Point(Latitude.unsafeMake(0.degrees), Longitude.unsafeMake(0.degrees))
    val p2_22 = Point(Latitude.unsafeMake(2.degrees), Longitude.unsafeMake(2.degrees))
    val q_11  = Point(Latitude.unsafeMake(1.degrees), Longitude.unsafeMake(1.degrees)) // On segment p1-p2

    val r_33  = Point(Latitude.unsafeMake(3.degrees), Longitude.unsafeMake(3.degrees)) // Collinear but outside p1-p2
    val s_12  = Point(Latitude.unsafeMake(1.degrees), Longitude.unsafeMake(2.degrees)) // Not collinear

    // Point on segment
    assert(Point.onSegment(p1_00, q_11, p2_22), "Point (1,1) should be on segment (0,0)-(2,2)")

    // Point on segment (endpoints)
    assert(Point.onSegment(p1_00, p1_00, p2_22), "Endpoint p1 (0,0) should be on segment (0,0)-(2,2)")
    assert(Point.onSegment(p1_00, p2_22, p2_22), "Endpoint p2 (2,2) should be on segment (0,0)-(2,2)")

    // Point collinear but outside segment
    assert(!Point.onSegment(p1_00, r_33, p2_22), "Point (3,3) should not be on segment (0,0)-(2,2)")
    val r_neg1_neg1 = Point(Latitude.unsafeMake(-1.degrees), Longitude.unsafeMake(-1.degrees))
    assert(!Point.onSegment(p1_00, r_neg1_neg1, p2_22), "Point (-1,-1) should not be on segment (0,0)-(2,2)")

    // Point not collinear with segment
    assert(!Point.onSegment(p1_00, s_12, p2_22), "Point (1,2) should not be on segment (0,0)-(2,2)")

    // Horizontal segment
    val h_p1_00 = Point(Latitude.unsafeMake(0.degrees), Longitude.unsafeMake(0.degrees))
    val h_p2_20 = Point(Latitude.unsafeMake(0.degrees), Longitude.unsafeMake(2.degrees))
    val h_q_10  = Point(Latitude.unsafeMake(0.degrees), Longitude.unsafeMake(1.degrees))
    assert(Point.onSegment(h_p1_00, h_q_10, h_p2_20), "Point (0,1) should be on horizontal segment (0,0)-(0,2)")
    val h_r_30  = Point(Latitude.unsafeMake(0.degrees), Longitude.unsafeMake(3.degrees))
    assert(!Point.onSegment(h_p1_00, h_r_30, h_p2_20), "Point (0,3) should not be on horizontal segment (0,0)-(0,2)")

    // Vertical segment
    val v_p1_00 = Point(Latitude.unsafeMake(0.degrees), Longitude.unsafeMake(0.degrees))
    val v_p2_02 = Point(Latitude.unsafeMake(2.degrees), Longitude.unsafeMake(0.degrees))
    val v_q_01  = Point(Latitude.unsafeMake(1.degrees), Longitude.unsafeMake(0.degrees))
    assert(Point.onSegment(v_p1_00, v_q_01, v_p2_02), "Point (1,0) should be on vertical segment (0,0)-(2,0)")
    val v_r_03  = Point(Latitude.unsafeMake(3.degrees), Longitude.unsafeMake(0.degrees))
    assert(!Point.onSegment(v_p1_00, v_r_03, v_p2_02), "Point (3,0) should not be on vertical segment (0,0)-(2,0)")
  }

  // --- Box Tests ---
  test("Box creation and basic properties") {
    val pNE = Point(Latitude.unsafeMake(10.degrees), Longitude.unsafeMake(20.degrees))
    val pSW = Point(Latitude.unsafeMake(0.degrees), Longitude.unsafeMake(10.degrees))
    val box = Box(pNE, pSW)

    assertEquals(box.northEast, pNE, "NorthEast point should match constructor argument")
    assertEquals(box.southWest, pSW, "SouthWest point should match constructor argument")

    assertEquals(box.north, Latitude.unsafeMake(10.degrees), "North latitude should match NE point's latitude")
    assertEquals(box.south, Latitude.unsafeMake(0.degrees), "South latitude should match SW point's latitude")
    assertEquals(box.east, Longitude.unsafeMake(20.degrees), "East longitude should match NE point's longitude")
    assertEquals(box.west, Longitude.unsafeMake(10.degrees), "West longitude should match SW point's longitude")
  }

  test("Box.area should calculate the correct area") {
    val pNE = Point(Latitude.unsafeMake(10.degrees), Longitude.unsafeMake(20.degrees)) // north = 10, east = 20
    val pSW = Point(Latitude.unsafeMake(0.degrees), Longitude.unsafeMake(10.degrees))  // south = 0, west = 10
    val box = Box(pNE, pSW)

    // height = north - south = 10.degrees - 0.degrees = 10.degrees
    // width: (east - west) = (20 - 10) = 10.degrees. (west - east) = -10.degrees.
    //        condition (10 < -10) is false. So, width = west - east = -10.degrees
    // area = SquareMeters(1) * (width.toDegrees * height.toDegrees)
    //      = SquareMeters(1) * (-10.0 * 10.0) = SquareMeters(-100.0)
    val expectedArea = (100.0).squareMeters
    assertEquals(box.area, expectedArea, "Area should be width.toDegrees * height.toDegrees")

    // Test with a zero-width box
    val pNE_zero_width = Point(Latitude.unsafeMake(10.degrees), Longitude.unsafeMake(10.degrees)) // north = 10, east = 10
    val pSW_zero_width = Point(Latitude.unsafeMake(0.degrees), Longitude.unsafeMake(10.degrees))  // south = 0, west = 10
    val box_zero_width = Box(pNE_zero_width, pSW_zero_width)
    // height = 10.degrees
    // width: (east - west) = (10 - 10) = 0.degrees. (west - east) = 0.degrees.
    //        condition (0 < 0) is false. So, width = west - east = 0.degrees
    // area = SquareMeters(1) * (0.0 * 10.0) = SquareMeters(0.0)
    assertEquals(box_zero_width.area, 0.0.squareMeters, "Area of zero-width box should be 0")

    // Test with a zero-height box
    val pNE_zero_height = Point(Latitude.unsafeMake(10.degrees), Longitude.unsafeMake(20.degrees)) // north = 10, east = 20
    val pSW_zero_height = Point(Latitude.unsafeMake(10.degrees), Longitude.unsafeMake(10.degrees))  // south = 10, west = 10
    val box_zero_height = Box(pNE_zero_height, pSW_zero_height)
    // height = 0.degrees
    // width = -10.degrees (as per first case logic)
    // area = SquareMeters(1) * (-10.0 * 0.0) = SquareMeters(0.0)
    assertEquals(box_zero_height.area, 0.0.squareMeters, "Area of zero-height box should be 0")
  }

  test("Box.width and Box.height should return correct angular differences") {
    // TODO: Add tests for Box.width and Box.height
  }

  test("Box.isSquare and Box.isPoint") {
    // TODO: Add tests for Box.isSquare and Box.isPoint
  }

  test("Box.distanceSquared should calculate squared Cartesian distance") {
    // TODO: Add tests for Box.distanceSquared
    // Note: This is likely a simplified Cartesian distance, not geodesic.
  }

  test("Box.southWestCorner and Box.northEastCorner") {
    // TODO: Add tests for corner accessors
  }

  test("Box.contains should correctly check for containment of another Box") {
    // Standard box (does not cross antimeridian)
    val boxStd = Box(Point(Latitude.unsafeMake(10.degrees), Longitude.unsafeMake(20.degrees)), Point(Latitude.unsafeMake(0.degrees), Longitude.unsafeMake(10.degrees))) // N:10, S:0, E:20, W:10, Width:10

    // Antimeridian crossing box
    val boxAM = Box(Point(Latitude.unsafeMake(10.degrees), Longitude.unsafeMake(-170.degrees)), Point(Latitude.unsafeMake(0.degrees), Longitude.unsafeMake(170.degrees))) // N:10, S:0, E:-170(190), W:170, Width:20

    // Test cases for boxStd
    val identicalToStd = Box(Point(Latitude.unsafeMake(10.degrees), Longitude.unsafeMake(20.degrees)), Point(Latitude.unsafeMake(0.degrees), Longitude.unsafeMake(10.degrees)))
    assert(boxStd.contains(identicalToStd), "boxStd should contain identical box")

    val smallerInStd = Box(Point(Latitude.unsafeMake(8.degrees), Longitude.unsafeMake(18.degrees)), Point(Latitude.unsafeMake(2.degrees), Longitude.unsafeMake(12.degrees))) // N:8,S:2,E:18,W:12, W:6
    assert(boxStd.contains(smallerInStd), "boxStd should contain smaller box inside")

    val largerThanStd = Box(Point(Latitude.unsafeMake(12.degrees), Longitude.unsafeMake(22.degrees)), Point(Latitude.unsafeMake(-5.degrees), Longitude.unsafeMake(5.degrees))) // N:15,S:-5,E:30,W:5, W:25
    assert(!boxStd.contains(largerThanStd), "boxStd should NOT contain larger box")

    val partiallyOverlappingStd = Box(Point(Latitude.unsafeMake(5.degrees), Longitude.unsafeMake(15.degrees)), Point(Latitude.unsafeMake(-5.degrees), Longitude.unsafeMake(5.degrees))) // N:5,S:-5,E:15,W:5, W:10
    assert(!boxStd.contains(partiallyOverlappingStd), "boxStd should NOT contain partially overlapping box (south outside)")

    val disjointFromStd = Box(Point(Latitude.unsafeMake(30.degrees), Longitude.unsafeMake(40.degrees)), Point(Latitude.unsafeMake(20.degrees), Longitude.unsafeMake(30.degrees)))
    assert(!boxStd.contains(disjointFromStd), "boxStd should NOT contain disjoint box")

    val touchingInternallyStd = Box(Point(Latitude.unsafeMake(10.degrees), Longitude.unsafeMake(15.degrees)), Point(Latitude.unsafeMake(0.degrees), Longitude.unsafeMake(12.degrees))) // N:10,S:0,E:15,W:12, W:3
    assert(boxStd.contains(touchingInternallyStd), "boxStd should contain internally touching box")

    val touchingExternallyStd = Box(Point(Latitude.unsafeMake(10.degrees), Longitude.unsafeMake(30.degrees)), Point(Latitude.unsafeMake(0.degrees), Longitude.unsafeMake(20.degrees))) // N:10,S:0,E:30,W:20, W:10
    assert(!boxStd.contains(touchingExternallyStd), "boxStd should NOT contain externally touching box")

    // Test cases for boxAM (antimeridian)
    val identicalToAM = Box(Point(Latitude.unsafeMake(10.degrees), Longitude.unsafeMake(-170.degrees)), Point(Latitude.unsafeMake(0.degrees), Longitude.unsafeMake(170.degrees)))
    assert(boxAM.contains(identicalToAM), "boxAM should contain identical box")

    val smallerInAM = Box(Point(Latitude.unsafeMake(8.degrees), Longitude.unsafeMake(-175.degrees)), Point(Latitude.unsafeMake(2.degrees), Longitude.unsafeMake(175.degrees))) // N:8,S:2,E:-175,W:175, W:10
    assert(boxAM.contains(smallerInAM), "boxAM should contain smaller box inside (AM)")

    val largerThanAM = Box(Point(Latitude.unsafeMake(12.degrees), Longitude.unsafeMake(-160.degrees)), Point(Latitude.unsafeMake(-2.degrees), Longitude.unsafeMake(160.degrees))) // N:15,S:-5,E:-160,W:160, W:40
    assert(!boxAM.contains(largerThanAM), "boxAM should NOT contain larger box (AM)")

    // This box (W:175, E:10) does NOT cross antimeridian. It's a small strip in the Eastern Hemisphere.
    val notCrossingAMButWithinLongitudesOfBoxAM = Box(Point(Latitude.unsafeMake(5.degrees), Longitude.unsafeMake(10.degrees)), Point(Latitude.unsafeMake(2.degrees), Longitude.unsafeMake(175.degrees)))
    // boxAM: W:170, E:-170 (spans 20 deg across AM). This box: W:175, E:10 (spans 15 deg, does not cross AM)
    // Lat: 10>=5, 0<=2 -> True
    // Lon for boxAM (W:170, E:-170): isLongitudeInsideThis(175) -> T, isLongitudeInsideThis(10) -> F
    assert(!boxAM.contains(notCrossingAMButWithinLongitudesOfBoxAM), "boxAM should NOT contain box that is technically within its lon range but doesn't cross AM itself if boxAM does")

    val widerThanAM = Box(Point(Latitude.unsafeMake(5.degrees), Longitude.unsafeMake(178.degrees)), Point(Latitude.unsafeMake(2.degrees), Longitude.unsafeMake(-160.degrees))) // N:5,S:2,E:160,W:-160 (spans 40deg, but other way around)
                                                                                      // This is actually E:-160 (200), W:160. Width is 40.
    assert(!boxAM.contains(widerThanAM), "boxAM should NOT contain a box that is wider (AM)")

    // Box that is contained by boxStd, but itself crosses antimeridian
    val amBoxInsideStd = Box(Point(Latitude.unsafeMake(5.degrees), Longitude.unsafeMake(-170.degrees)), Point(Latitude.unsafeMake(2.degrees), Longitude.unsafeMake(172.degrees))) // N:5,S:2,E:-170,W:170, W:20
    // boxStd: W:10, E:20. isLongitudeInsideThis(170) -> F. isLongitudeInsideThis(-170) -> F.
    assert(!boxStd.contains(amBoxInsideStd), "boxStd should NOT contain a box that crosses AM if std box does not")

    // Box that contains boxStd, but itself crosses antimeridian
    val amBoxContainsStd = Box(Point(Latitude.unsafeMake(15.degrees), Longitude.unsafeMake(9.degrees)), Point(Latitude.unsafeMake(-5.degrees), Longitude.unsafeMake(21.degrees))) // N:15,S:-5,E:9,W:21 (crosses AM, e.g. 21 to 360 then 0 to 9)
                                                                                          // E:9 (East), W:21 (West) means it crosses AM. Width (21..360..9) = (360-21)+9 = 348 degrees.
    // boxStd: N:10,S:0,E:20,W:10
    // Lat: 15>=10, -5<=0 -> T
    // Lon for amBoxContainsStd (W:21, E:9): isLongitudeInsideThis(10) -> T, isLongitudeInsideThis(20) -> T
    // Width: amBoxContainsStd.width (348) >= boxStd.width (10) -> T
    assert(amBoxContainsStd.contains(boxStd), "AM-crossing box (very wide) should contain boxStd")

    val amBoxContainsStdNarrow = Box(Point(Latitude.unsafeMake(15.degrees), Longitude.unsafeMake(25.degrees)), Point(Latitude.unsafeMake(-5.degrees), Longitude.unsafeMake(5.degrees))) // N:15,S:-5,E:25,W:5 (normal box)
    // This was the old `containerCand`. This is a normal box, W:5, E:25. Width 20.
    // boxStd: N:10,S:0,E:20,W:10
    // amBoxContainsStdNarrow.contains(boxStd)
    // Lat: 15>=10, -5<=0 -> T
    // Lon for amBoxContainsStdNarrow (W:5, E:25): isLongitudeInsideThis(10) -> T, isLongitudeInsideThis(20) -> T
    // Width: amBoxContainsStdNarrow.width (20) >= boxStd.width (10) -> T
    assert(amBoxContainsStdNarrow.contains(boxStd), "Large non-AM box should contain boxStd")
  }

  test("Box.expand should create a new Box enclosing both original boxes") {
    val lat0 = Latitude.unsafeMake(0.degrees)
    val lat5 = Latitude.unsafeMake(5.degrees)
    val lat10 = Latitude.unsafeMake(10.degrees)
    val lat15 = Latitude.unsafeMake(15.degrees)
    val lat20 = Latitude.unsafeMake(20.degrees)
    val lat30 = Latitude.unsafeMake(30.degrees)

    val lon0 = Longitude.unsafeMake(0.degrees)
    val lon10 = Longitude.unsafeMake(10.degrees)
    val lon20 = Longitude.unsafeMake(20.degrees)
    val lon25 = Longitude.unsafeMake(25.degrees)
    val lon30 = Longitude.unsafeMake(30.degrees)
    val lon40 = Longitude.unsafeMake(40.degrees)

    // Test Case 1: Non-overlapping boxes
    val box1_ne = Point(lat10, lon20)
    val box1_sw = Point(lat0, lon10)
    val box1 = Box(box1_ne, box1_sw)

    val box2_ne = Point(lat30, lon40)
    val box2_sw = Point(lat20, lon30)
    val box2 = Box(box2_ne, box2_sw)

    val expanded1 = box1.expand(box2)
    val expected1_ne = Point(lat30, lon40) // max(10,30), max(20,40)
    val expected1_sw = Point(lat0, lon10)   // min(0,20),  min(10,30)
    assertEquals(expanded1.northEast, expected1_ne, "Non-overlapping: NE point mismatch")
    assertEquals(expanded1.southWest, expected1_sw, "Non-overlapping: SW point mismatch")

    // Test Case 2: One box containing another (box1 contains box2)
    val outer_ne = Point(lat20, lon30)
    val outer_sw = Point(lat0, lon0)
    val outerBox = Box(outer_ne, outer_sw)

    val inner_ne = Point(lat15, lon20)
    val inner_sw = Point(lat5, lon10)
    val innerBox = Box(inner_ne, inner_sw)

    val expanded2 = outerBox.expand(innerBox)
    assertEquals(expanded2.northEast, outer_ne, "Outer contains Inner: NE point mismatch")
    assertEquals(expanded2.southWest, outer_sw, "Outer contains Inner: SW point mismatch")

    val expanded3 = innerBox.expand(outerBox) // Expand inner with outer
    assertEquals(expanded3.northEast, outer_ne, "Inner expanded by Outer: NE point mismatch")
    assertEquals(expanded3.southWest, outer_sw, "Inner expanded by Outer: SW point mismatch")

    // Test Case 3: Overlapping boxes
    // box1: NE(10N, 20E), SW(0N, 0E)
    // box3: NE(15N, 25E), SW(5N, 10E)
    val boxA = Box(Point(lat10, lon20), Point(lat0, lon0))
    val boxB = Box(Point(lat15, lon25), Point(lat5, lon10))
    val expanded4 = boxA.expand(boxB)
    val expected4_ne = Point(lat15, lon25) // max(10,15), max(20,25)
    val expected4_sw = Point(lat0, lon0)   // min(0,5),   min(0,10)
    assertEquals(expanded4.northEast, expected4_ne, "Overlapping: NE point mismatch")
    assertEquals(expanded4.southWest, expected4_sw, "Overlapping: SW point mismatch")

    // Test Case 4: Expanding a box with itself
    val expanded5 = box1.expand(box1)
    assertEquals(expanded5.northEast, box1.northEast, "Expand with self: NE point mismatch")
    assertEquals(expanded5.southWest, box1.southWest, "Expand with self: SW point mismatch")

    // Test Case 5: Antimeridian crossing
    // BoxX: NE(10N, 170E), SW(0N, 160E)
    // BoxY: NE(10N, -170E (190E)), SW(0N, 175E)
    // Expected: NE(10N, -170E), SW(0N, 160E)
    val lon160E = Longitude.unsafeMake(160.degrees)
    val lon170E = Longitude.unsafeMake(170.degrees)
    val lon175E = Longitude.unsafeMake(175.degrees)
    val lon_neg170E = Longitude.unsafeMake(-170.degrees) // East of 180, effectively 190 degrees

    val boxX = Box(Point(lat10, lon170E), Point(lat0, lon160E))
    val boxY = Box(Point(lat10, lon_neg170E), Point(lat0, lon175E))

    val expandedAntiM = boxX.expand(boxY)
    val expectedAntiM_ne = Point(lat10, lon_neg170E) // max(10,10), max(170E, -170E) = -170E
    val expectedAntiM_sw = Point(lat0, lon160E)   // min(0,0),   min(160E, 175E) = 160E

    assertEquals(expandedAntiM.northEast, expectedAntiM_ne, "Antimeridian: NE point mismatch")
    assertEquals(expandedAntiM.southWest, expectedAntiM_sw, "Antimeridian: SW point mismatch")
  }

  test("Box.expandArea - current behavior") {
    // TODO: Add tests for Box.expandArea
    // Note: The comment in ShapesCore.scala indicates this calculation is likely incorrect for geodesic areas.
    // We will test its current arithmetic behavior.
  }

  test("Box.intersects should correctly check for intersection with another Box") {
    // TODO: Add tests for Box.intersects
  }

}
