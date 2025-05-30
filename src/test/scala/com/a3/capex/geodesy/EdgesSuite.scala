package com.a3.capex.geodesy

import munit.FunSuite
import com.a3.capex.geodesy.Edges.Edge
import com.a3.capex.geodesy.ShapesCore.{Point, Box}
import com.a3.capex.geodesy.Coordinates.{Latitude, Longitude}
import squants.space.AngleConversions._ // For .degrees

class EdgesSuite extends FunSuite {

  test("Edge creation and basic properties") {
    val p1 = Point(Latitude.unsafeMake(10.degrees), Longitude.unsafeMake(20.degrees))
    val p2 = Point(Latitude.unsafeMake(30.degrees), Longitude.unsafeMake(40.degrees))
    
    val edge1 = Edge(p1, p2)
    assertEquals(edge1.origin, p1)
    assertEquals(edge1.end, p2)
    assert(!edge1.isDegenerate, "Edge p1-p2 should not be degenerate")

    val edge2 = Edge(p1, p1)
    assert(edge2.isDegenerate, "Edge p1-p1 should be degenerate")
  }

  test("Edge.barycenter should calculate the arithmetic mean of coordinates") {
    val lat1 = Latitude.unsafeMake(10.degrees)
    val lon1 = Longitude.unsafeMake(20.degrees)
    val p1 = Point(lat1, lon1)

    val lat2 = Latitude.unsafeMake(30.degrees)
    val lon2 = Longitude.unsafeMake(40.degrees)
    val p2 = Point(lat2, lon2)

    val edge1 = Edge(p1, p2)
    val expectedBarycenter1 = Point(
      Latitude.unsafeMake((10.degrees + 30.degrees) / 2.0),
      Longitude.unsafeMake((20.degrees + 40.degrees) / 2.0)
    )
    assertEquals(edge1.barycenter, expectedBarycenter1, s"Barycenter for ${edge1} was not ${expectedBarycenter1}")

    // Test with negative values
    val lat3 = Latitude.unsafeMake(-10.degrees)
    val lon3 = Longitude.unsafeMake(-20.degrees)
    val p3 = Point(lat3, lon3)

    val lat4 = Latitude.unsafeMake(-50.degrees)
    val lon4 = Longitude.unsafeMake(-60.degrees)
    val p4 = Point(lat4, lon4)

    val edge2 = Edge(p3, p4)
    val expectedBarycenter2 = Point(
      Latitude.unsafeMake((-10.degrees + -50.degrees) / 2.0),
      Longitude.unsafeMake((-20.degrees + -60.degrees) / 2.0)
    )
    assertEquals(edge2.barycenter, expectedBarycenter2, s"Barycenter for ${edge2} was not ${expectedBarycenter2}")

    // Test longitude behavior (arithmetic mean of normalized angles)
    val lon5 = Longitude.unsafeMake(170.degrees) // p5 at (0, 170)
    val p5 = Point(lat1, lon5) // Use lat1 for simplicity
    val lon6 = Longitude.unsafeMake(-170.degrees) // p6 at (0, -170), which is 190 deg
    val p6 = Point(lat1, lon6)
    
    val edge3 = Edge(p5, p6)
    // Expected: (Angle(170) + Angle(-170)) / 2 = 0. Then Longitude.unsafeMake(0)
    val expectedBarycenter3 = Point(
      lat1, // Latitude remains the same as it's (10+10)/2
      Longitude.unsafeMake(0.degrees) 
    )
    assertEquals(edge3.barycenter, expectedBarycenter3, s"Barycenter for ${edge3} was not ${expectedBarycenter3}")
  }

  test("Edge geographic properties (north, south, east, west) should return correct coordinates") {
    val lat1 = Latitude.unsafeMake(50.degrees) // North
    val lon1 = Longitude.unsafeMake(10.degrees) // East
    val pOrigin = Point(lat1, lon1)

    val lat2 = Latitude.unsafeMake(20.degrees) // South
    val lon2 = Longitude.unsafeMake(-5.degrees) // West
    val pEnd = Point(lat2, lon2)

    val edge = Edge(pOrigin, pEnd)

    assertEquals(edge.north, lat1, "North should be origin.latitude")
    assertEquals(edge.south, lat2, "South should be end.latitude")
    assertEquals(edge.east, lon1, "East should be origin.longitude")
    assertEquals(edge.west, lon2, "West should be end.longitude")
  }

  test("Edge.contains should always return false") {
    val p1 = Point(Latitude.unsafeMake(10.degrees), Longitude.unsafeMake(20.degrees))
    val p2 = Point(Latitude.unsafeMake(30.degrees), Longitude.unsafeMake(40.degrees))
    val edge = Edge(p1, p2)
    val dummyBox = Box(p1, p2) // Box content doesn't matter for this test
    assert(!edge.contains(dummyBox), "Edge.contains should always be false")
  }

  test("Edge.sorted should sort points by latitude, then longitude") {
    val lat1 = Latitude.unsafeMake(10.degrees)
    val lon1 = Longitude.unsafeMake(20.degrees)
    val p1_lat1_lon1 = Point(lat1, lon1)

    val lat2 = Latitude.unsafeMake(5.degrees) // Smaller latitude
    val p2_lat2_lon1 = Point(lat2, lon1)

    val lon2 = Longitude.unsafeMake(15.degrees) // Smaller longitude
    val p3_lat1_lon2 = Point(lat1, lon2)

    val edgeUnsortedLat = Edge(p1_lat1_lon1, p2_lat2_lon1) // lat1 > lat2
    val edgeSortedLat = Edge(p2_lat2_lon1, p1_lat1_lon1)
    assertEquals(edgeUnsortedLat.sorted(), edgeSortedLat, "Edge should be sorted by latitude first")

    val edgeUnsortedLon = Edge(p1_lat1_lon1, p3_lat1_lon2) // lat1 == lat1, lon1 > lon2
    val edgeSortedLon = Edge(p3_lat1_lon2, p1_lat1_lon1)
    assertEquals(edgeUnsortedLon.sorted(), edgeSortedLon, "Edge should be sorted by longitude if latitudes are equal")

    // Already sorted
    assertEquals(edgeSortedLat.sorted(), edgeSortedLat, "Already sorted edge (by lat) should remain unchanged")
    assertEquals(edgeSortedLon.sorted(), edgeSortedLon, "Already sorted edge (by lon) should remain unchanged")

    // Identical points (degenerate edge)
    val degenerateEdge = Edge(p1_lat1_lon1, p1_lat1_lon1)
    assertEquals(degenerateEdge.sorted(), degenerateEdge, "Sorted degenerate edge should remain unchanged")
  }

  test("Edge.doIntersect - general cases") {
    // Case 1: General intersection
    // Edge 1: (0,0) -> (2,2)
    // Edge 2: (0,2) -> (2,0)
    val p1_e1 = Point(Latitude.unsafeMake(0.degrees), Longitude.unsafeMake(0.degrees))
    val p2_e1 = Point(Latitude.unsafeMake(2.degrees), Longitude.unsafeMake(2.degrees))
    val edge1 = Edge(p1_e1, p2_e1)

    val p1_e2 = Point(Latitude.unsafeMake(0.degrees), Longitude.unsafeMake(2.degrees))
    val p2_e2 = Point(Latitude.unsafeMake(2.degrees), Longitude.unsafeMake(0.degrees))
    val edge2 = Edge(p1_e2, p2_e2)
    assert(edge1.doIntersect(edge2), "Edges ((0,0)-(2,2)) and ((0,2)-(2,0)) should intersect")
    assert(edge2.doIntersect(edge1), "Intersection should be commutative")

    // Case 2: No intersection (parallel)
    // Edge 3: (0,0) -> (2,0)
    // Edge 4: (0,1) -> (2,1)
    val p1_e3 = Point(Latitude.unsafeMake(0.degrees), Longitude.unsafeMake(0.degrees))
    val p2_e3 = Point(Latitude.unsafeMake(2.degrees), Longitude.unsafeMake(0.degrees))
    val edge3 = Edge(p1_e3, p2_e3)

    val p1_e4 = Point(Latitude.unsafeMake(0.degrees), Longitude.unsafeMake(1.degrees))
    val p2_e4 = Point(Latitude.unsafeMake(2.degrees), Longitude.unsafeMake(1.degrees))
    val edge4 = Edge(p1_e4, p2_e4)
    assert(!edge3.doIntersect(edge4), "Parallel edges ((0,0)-(2,0)) and ((0,1)-(2,1)) should not intersect")

    // Case 3: No intersection (far apart)
    // Edge 5: (0,0) -> (1,1)
    // Edge 6: (10,10) -> (11,11)
    val p1_e5 = Point(Latitude.unsafeMake(0.degrees), Longitude.unsafeMake(0.degrees))
    val p2_e5 = Point(Latitude.unsafeMake(1.degrees), Longitude.unsafeMake(1.degrees))
    val edge5 = Edge(p1_e5, p2_e5)

    val p1_e6 = Point(Latitude.unsafeMake(10.degrees), Longitude.unsafeMake(10.degrees))
    val p2_e6 = Point(Latitude.unsafeMake(11.degrees), Longitude.unsafeMake(11.degrees))
    val edge6 = Edge(p1_e6, p2_e6)
    assert(!edge5.doIntersect(edge6), "Far apart edges should not intersect")
  }

  test("Edge.doIntersect - collinear cases") {
    // Points for collinear tests (y = x line)
    val p00 = Point(Latitude.unsafeMake(0.degrees), Longitude.unsafeMake(0.degrees))
    val p11 = Point(Latitude.unsafeMake(1.degrees), Longitude.unsafeMake(1.degrees))
    val p22 = Point(Latitude.unsafeMake(2.degrees), Longitude.unsafeMake(2.degrees))
    val p33 = Point(Latitude.unsafeMake(3.degrees), Longitude.unsafeMake(3.degrees))
    val pMinus11 = Point(Latitude.unsafeMake(-1.degrees), Longitude.unsafeMake(-1.degrees))

    // Case 1: Collinear and overlapping
    // Edge A: (0,0) -> (2,2)
    // Edge B: (1,1) -> (3,3)
    val edgeA_00_22 = Edge(p00, p22)
    val edgeB_11_33 = Edge(p11, p33)
    assert(edgeA_00_22.doIntersect(edgeB_11_33), "Collinear overlapping edges ((0,0)-(2,2)) and ((1,1)-(3,3)) should intersect")
    assert(edgeB_11_33.doIntersect(edgeA_00_22), "Intersection should be commutative for collinear overlapping")

    // Case 2: Collinear and non-overlapping (gap in between)
    // Edge C: (0,0) -> (1,1)
    // Edge D: (2,2) -> (3,3)
    val edgeC_00_11 = Edge(p00, p11)
    val edgeD_22_33 = Edge(p22, p33)
    assert(!edgeC_00_11.doIntersect(edgeD_22_33), "Collinear non-overlapping edges ((0,0)-(1,1)) and ((2,2)-(3,3)) should not intersect")

    // Case 3: Collinear, one contains the other
    // Edge E: (0,0) -> (3,3)
    // Edge F: (1,1) -> (2,2)
    val edgeE_00_33 = Edge(p00, p33)
    val edgeF_11_22 = Edge(p11, p22)
    assert(edgeE_00_33.doIntersect(edgeF_11_22), "Collinear edge ((0,0)-(3,3)) containing ((1,1)-(2,2)) should intersect")
    assert(edgeF_11_22.doIntersect(edgeE_00_33), "Intersection should be commutative for collinear containment")

    // Case 4: Collinear, touching at one endpoint
    // Edge G: (0,0) -> (1,1)
    // Edge H: (1,1) -> (2,2)
    val edgeG_00_11 = Edge(p00, p11)
    val edgeH_11_22 = Edge(p11, p22)
    assert(edgeG_00_11.doIntersect(edgeH_11_22), "Collinear edges ((0,0)-(1,1)) and ((1,1)-(2,2)) touching at endpoint should intersect")
    assert(edgeH_11_22.doIntersect(edgeG_00_11), "Intersection should be commutative for collinear endpoint touch")

    // Case 5: Collinear, sharing an endpoint, one reversed
    // Edge I: (0,0) -> (1,1)
    // Edge J: (2,2) -> (1,1) (reversed compared to H)
    val edgeI_00_11 = Edge(p00, p11)
    val edgeJ_22_11 = Edge(p22, p11)
    assert(edgeI_00_11.doIntersect(edgeJ_22_11), "Collinear edges ((0,0)-(1,1)) and ((2,2)-(1,1)) sharing endpoint (one reversed) should intersect")
    assert(edgeJ_22_11.doIntersect(edgeI_00_11), "Intersection should be commutative for collinear shared endpoint (one reversed)")

    // Case 6: Collinear, non-overlapping, one point to the left
    // Edge K: (-1,-1) -> (0,0)
    // Edge L: (1,1) -> (2,2)
    val edgeK_m11_00 = Edge(pMinus11, p00)
    val edgeL_11_22 = Edge(p11, p22)
    assert(!edgeK_m11_00.doIntersect(edgeL_11_22), "Collinear non-overlapping edges ((-1,-1)-(0,0)) and ((1,1)-(2,2)) should not intersect")
  }

  test("Edge.doIntersect - T-intersections and degenerate edges") {
    // Points for T-intersections
    val p01 = Point(Latitude.unsafeMake(0.degrees), Longitude.unsafeMake(1.degrees))
    val p10 = Point(Latitude.unsafeMake(1.degrees), Longitude.unsafeMake(0.degrees))
    val p11 = Point(Latitude.unsafeMake(1.degrees), Longitude.unsafeMake(1.degrees))
    val p12 = Point(Latitude.unsafeMake(1.degrees), Longitude.unsafeMake(2.degrees))
    val p21 = Point(Latitude.unsafeMake(2.degrees), Longitude.unsafeMake(1.degrees))
    val p31 = Point(Latitude.unsafeMake(3.degrees), Longitude.unsafeMake(1.degrees))

    // Case 1: T-intersection - one edge's endpoint lies on the other edge
    // Edge T1: (0,1) -> (2,1)
    // Edge T2: (1,0) -> (1,1) (endpoint (1,1) is on T1)
    val edgeT1_01_21 = Edge(p01, p21)
    val edgeT2_10_11 = Edge(p10, p11)
    assert(edgeT1_01_21.doIntersect(edgeT2_10_11), "T-intersection ((0,1)-(2,1)) and ((1,0)-(1,1)) should intersect")
    assert(edgeT2_10_11.doIntersect(edgeT1_01_21), "Intersection should be commutative for T-intersection (endpoint on edge)")

    // Case 2: Standard T-junction (crossing, not just endpoint)
    // Edge T3: (0,1) -> (3,1)
    // Edge T4: (1,0) -> (1,2) (T4 crosses T3 at (1,1))
    val edgeT3_01_31 = Edge(p01, p31)
    val edgeT4_10_12 = Edge(p10, p12)
    assert(edgeT3_01_31.doIntersect(edgeT4_10_12), "Standard T-junction ((0,1)-(3,1)) and ((1,0)-(1,2)) should intersect")
    assert(edgeT4_10_12.doIntersect(edgeT3_01_31), "Intersection should be commutative for standard T-junction")

    // Points for degenerate edge tests
    val p00 = Point(Latitude.unsafeMake(0.degrees), Longitude.unsafeMake(0.degrees))
    val p22 = Point(Latitude.unsafeMake(2.degrees), Longitude.unsafeMake(2.degrees))
    val p33 = Point(Latitude.unsafeMake(3.degrees), Longitude.unsafeMake(3.degrees))

    // Degenerate Edges
    val degenP11 = Edge(p11, p11) // Degenerate edge at (1,1)
    val degenP33 = Edge(p33, p33) // Degenerate edge at (3,3)

    // Normal Edge for testing against degenerate edges
    val normalEdge_00_22 = Edge(p00, p22) // (0,0) -> (2,2)

    // Case 3: Degenerate edge (point) lies on non-degenerate edge
    // Degenerate D1: (1,1)-(1,1)
    // Normal N1: (0,0)-(2,2)
    assert(degenP11.doIntersect(normalEdge_00_22), "Degenerate edge (1,1) on normal edge ((0,0)-(2,2)) should intersect")
    assert(normalEdge_00_22.doIntersect(degenP11), "Intersection with degenerate edge should be commutative")

    // Case 4: Degenerate edge (point) does NOT lie on non-degenerate edge
    // Degenerate D2: (3,3)-(3,3)
    // Normal N1: (0,0)-(2,2)
    assert(!degenP33.doIntersect(normalEdge_00_22), "Degenerate edge (3,3) not on normal edge ((0,0)-(2,2)) should not intersect")

    // Case 5: Two degenerate edges at the same point
    // Degenerate D1: (1,1)-(1,1)
    // Degenerate D3: (1,1)-(1,1) (another instance)
    val degenP11_alt = Edge(p11, p11)
    assert(degenP11.doIntersect(degenP11_alt), "Two degenerate edges at the same point (1,1) should intersect")

    // Case 6: Two degenerate edges at different points
    // Degenerate D1: (1,1)-(1,1)
    // Degenerate D2: (3,3)-(3,3)
    assert(!degenP11.doIntersect(degenP33), "Two degenerate edges at different points (1,1) and (3,3) should not intersect")
  }
}
