package com.a3.capex.geodesy

import munit.FunSuite
import com.a3.capex.geodesy.ShapesCore.Point
import com.a3.capex.geodesy.Coordinates.{Latitude, Longitude, R}
import com.a3.capex.geodesy.Triangles.Triangle // Specific import for Triangle class and companion
import squants.space.AngleConversions.*
import squants.space.{Area, Kilometers, Length, Meters, SquareKilometers, SquareMeters}
import com.a3.capex.geodesy.TypeclassInstances.given // For Ordering if needed by Point.isSorted indirectly
import neotype.unwrap                                // For unwrapping Latitude/Longitude

class TrianglesSuite extends FunSuite {

  // Helper to create points easily
  def pt(latDeg: Double, lonDeg: Double): Point =
    Point(Latitude.unsafeMake(latDeg.degrees), Longitude.unsafeMake(lonDeg.degrees))

  // Epsilon for floating point comparisons (especially for Area and Length)
  val epsilonArea: Area     = SquareMeters(0.001)
  val epsilonLength: Length = Meters(0.001)

  test("Triangle creation and basic properties") {
    val p1  = pt(0, 0)
    val p2  = pt(0, 1)
    val p3  = pt(1, 0)
    val tri = Triangle(p1, p2, p3)

    assertEquals(tri.p1, p1)
    assertEquals(tri.p2, p2)
    assertEquals(tri.p3, p3)
    assertEquals(tri.toString, s"[$p1 , $p2 , $p3]")
  }

  test("Triangle.apply(Point) should create a degenerate triangle") {
    val p   = pt(10, 20)
    val tri = Triangle(p) // Companion object apply
    assert(tri.isDegenerate, "Triangle(p) should be degenerate")
    assertEquals(tri.p1, p)
    assertEquals(tri.p2, p)
    assertEquals(tri.p3, p)
    assert(
      Math.abs(tri.area.toSquareMeters - 0.0) <= epsilonArea.toSquareMeters,
      "Degenerate triangle area should be zero"
    )
  }

  test("isDegenerate should identify degenerate triangles") {
    val p1 = pt(0, 0)
    val p2 = pt(1, 1)
    val p3 = pt(2, 2) // Collinear
    val p4 = pt(0, 1)

    assert(Triangle(p1, p1, p2).isDegenerate, "p1,p1,p2 is degenerate")
    assert(Triangle(p1, p2, p1).isDegenerate, "p1,p2,p1 is degenerate")
    assert(Triangle(p2, p1, p1).isDegenerate, "p2,p1,p1 is degenerate")
    assert(Triangle(p1, p1, p1).isDegenerate, "p1,p1,p1 is degenerate")
    assert(!Triangle(p1, p4, pt(1, 0)).isDegenerate, "Non-degenerate triangle")
    // Note: isDegenerate currently checks for p1==p2 || p1==p3 || p2==p3.
    // It does not check for collinearity of distinct points for area calculation purposes.
    // The area calculation itself handles collinear distinct points by yielding zero area.
  }

  test("barycenter calculation") {
    val p1     = pt(0, 0)
    val p2     = pt(0, 3)
    val p3     = pt(3, 0)
    val tri    = Triangle(p1, p2, p3)
    val center = tri.barycenter
    // For these simple points, average is ( (0+0+3)/3, (0+3+0)/3 ) = (1, 1)
    assert(Math.abs(center.latitude.unwrap.toDegrees - 1.0) <= 1e-9, "Barycenter latitude")
    assert(Math.abs(center.longitude.unwrap.toDegrees - 1.0) <= 1e-9, "Barycenter longitude")
  }

  test("geographic bounds (north, south, east, west)") {
    val p1  = pt(10, 20)
    val p2  = pt(0, 30)
    val p3  = pt(5, 10)
    val tri = Triangle(p1, p2, p3)

    assertEquals(tri.north, Latitude.unsafeMake(10.degrees))
    assertEquals(tri.south, Latitude.unsafeMake(0.degrees))
    assertEquals(tri.east, Longitude.unsafeMake(30.degrees))
    assertEquals(tri.west, Longitude.unsafeMake(10.degrees))
  }

  test("geographic bounds with antimeridian crossing for longitude") {
    val p1 = pt(0, 170)   // East
    val p2 = pt(10, -170) // East (190 deg from 0, or -170)
    val p3 = pt(5, 175)   // East
    // Expected: west=170, east=-170 (normalized from 190)
    // The .max/.min on Longitude uses the custom Ordering[Longitude]
    val tri = Triangle(p1, p2, p3)

    assertEquals(tri.north, Latitude.unsafeMake(10.degrees))
    assertEquals(tri.south, Latitude.unsafeMake(0.degrees))
    // East/West for Longitude uses the custom Ordering which compares shortest path
    // So, -170 (or 190) is more easterly than 175 and 170.
    // And 170 is most westerly.
    assertEquals(tri.east, Longitude.unsafeMake(-170.degrees))
    assertEquals(tri.west, Longitude.unsafeMake(170.degrees))
  }

  test("sorted should order vertices correctly") {
    val p1 = pt(0, 1) // b
    val p2 = pt(1, 0) // c
    val p3 = pt(0, 0) // a
    // Expected sorted order: p3 (0,0), p1 (0,1), p2 (1,0)
    // Point.isSorted compares lat first, then lon.

    val tri       = Triangle(p1, p2, p3)
    val sortedTri = tri.sorted

    assertEquals(sortedTri.p1, p3, "Sorted p1 should be (0,0)")
    assertEquals(sortedTri.p2, p1, "Sorted p2 should be (0,1)")
    assertEquals(sortedTri.p3, p2, "Sorted p3 should be (1,0)")

    val alreadySortedTri = Triangle(p3, p1, p2)
    assertEquals(alreadySortedTri.sorted.p1, p3)
    assertEquals(alreadySortedTri.sorted.p2, p1)
    assertEquals(alreadySortedTri.sorted.p3, p2)
  }

  test("Equiv[Triangle] should compare triangles by their set of points") {
    val p1 = pt(0, 0)
    val p2 = pt(0, 1)
    val p3 = pt(1, 0)
    val p4 = pt(1, 1)

    val tri1 = Triangle(p1, p2, p3)
    val tri2 = Triangle(p3, p1, p2) // Same points, different order
    val tri3 = Triangle(p2, p3, p1) // Same points, different order
    val tri4 = Triangle(p1, p2, p4) // Different point

    val equiv = summon[Equiv[Triangle]]

    assert(equiv.equiv(tri1, tri2), "tri1 should be equivalent to tri2")
    assert(equiv.equiv(tri1, tri3), "tri1 should be equivalent to tri3")
    assert(!equiv.equiv(tri1, tri4), "tri1 should not be equivalent to tri4")
  }

  // --- Area Tests ---
  test("area of a degenerate triangle should be zero") {
    val p1 = pt(10, 10)
    assertEquals(Triangle(p1, p1, p1).area.toSquareMeters, 0.0, epsilonArea.toSquareMeters)
    assertEquals(Triangle(p1, p1, pt(20, 20)).area.toSquareMeters, 0.0, epsilonArea.toSquareMeters)
  }

  test("area of three distinct collinear points should be zero") {
    val p1 = pt(0, 0)
    val p2 = pt(1, 1) // y = x
    val p3 = pt(2, 2) // y = x
    // The area formula for spherical triangles should handle collinearity by resulting in zero spherical excess.
    assert(
      Math.abs(Triangle(p1, p2, p3).area.toSquareMeters - 0.0) <= epsilonArea.toSquareMeters,
      "Area of 3 distinct collinear points should be ~0"
    )
  }

  // Test based on a known example for spherical triangle area if possible or properties.
  // Example from Wikipedia: Octant of a sphere ( equilateral triangle with 90 deg angles on a sphere surface)
  // Angles: pi/2, pi/2, pi/2. Spherical Excess E = 3*pi/2 - pi = pi/2.
  // Area = E * R^2 = (pi/2) * R^2. This is 1/8 of the sphere's surface area (4*pi*R^2).
  // Let's define points that form such an octant.
  // p1 = (0,0) Equator, Prime Meridian
  // p2 = (0,90) Equator, 90E
  // p3 = (90,0) North Pole (lon is irrelevant but let's use 0)
  test("area of a spherical octant (1/8 of sphere surface)") {
    val p1  = pt(0, 0)
    val p2  = pt(0, 90)
    val p3  = pt(90, 0) // North Pole
    val tri = Triangle(p1, p2, p3)

    val expectedArea = (R * R * Math.PI / 2.0) // E * R^2 where E = pi/2
    // Convert expectedArea (which will be in SquareKilometers if R is in Kilometers)
    // to SquareMeters for comparison with tri.area.toSquareMeters
    val expectedAreaSqMeters = expectedArea.toSquareMeters

    // The area calculation in Triangle.scala uses R from Coordinates (6371 km)
    // It returns SquareMeters directly.
    assert(
      Math.abs(tri.area.toSquareMeters - expectedAreaSqMeters) <= (expectedAreaSqMeters * 0.001),
      "Area of spherical octant"
    ) // 0.1% tolerance
  }

  // --- Partitioning Tests ---
  test("midTriangles should produce 4 triangles") {
    val p1       = pt(0, 0); val p2 = pt(0, 2); val p3 = pt(2, 0)
    val tri      = Triangle(p1, p2, p3)
    val children = tri.midTriangles
    assertEquals(children.size, 4)

    // Check one of the child triangles' vertices
    val m12            = p1.middle(p2) // Midpoint of p1-p2: (0,1)
    val m13            = p1.middle(p3) // Midpoint of p1-p3: (1,0)
    val expectedChild1 = Triangle(p1, m12, m13)
    // Use Equiv for comparison as vertex order in midTriangles might not be exact
    assert(children.exists(summon[Equiv[Triangle]].equiv(_, expectedChild1)), "First midTriangle check")
  }

  test("partition(level) behavior") {
    val p1  = pt(0, 0); val p2 = pt(0, 2); val p3 = pt(2, 0)
    val tri = Triangle(p1, p2, p3)

    assertEquals(tri.partition(-1).size, 0, "partition(-1) should be empty")
    assertEquals(tri.partition(0).size, 1, "partition(0) should be List(this)")
    assert(summon[Equiv[Triangle]].equiv(tri.partition(0).head, tri), "partition(0).head should be tri")

    assertEquals(tri.partition(1).size, 4, "partition(1) should have 4 triangles")
    // Check if partition(1) is equivalent to midTriangles (set comparison for lists of triangles)
    val equiv           = summon[Equiv[Triangle]]
    val partition1Set   = tri.partition(1).map(t => Set(t.p1, t.p2, t.p3)).toSet
    val midTrianglesSet = tri.midTriangles.map(t => Set(t.p1, t.p2, t.p3)).toSet
    assertEquals(partition1Set, midTrianglesSet, "partition(1) should be equivalent to midTriangles")

    assertEquals(tri.partition(2).size, 16, "partition(2) should have 16 triangles (4*4)")
  }

  test("distance(Point) calculates distance from barycenter") {
    val p1   = pt(0, 0)
    val p2   = pt(0, 0.002)   // Approx 222m East if at equator
    val p3   = pt(0.002, 0)   // Approx 222m North
    val tri  = Triangle(p1, p2, p3)
    val bary = tri.barycenter // Approx (0.000666, 0.000666)

    val distToP1         = tri.distance(p1)
    val expectedDistToP1 = bary.distance(p1)
    assertEquals(distToP1.toMeters, expectedDistToP1.toMeters, epsilonLength.toMeters)

    val externalPt             = pt(10, 10)
    val distToExternal         = tri.distance(externalPt)
    val expectedDistToExternal = bary.distance(externalPt)
    assertEquals(distToExternal.toMeters, expectedDistToExternal.toMeters, epsilonLength.toMeters)
  }
}
