package com.a3.capex.geodesy

import munit.FunSuite
import com.a3.capex.geodesy.ShapesCore.Point
import com.a3.capex.geodesy.Coordinates.{Latitude, Longitude}
import squants.space.AngleConversions.*

class TrianglesServicesSuite extends FunSuite {

  // Helper to create points easily
  def pt(latDeg: Double, lonDeg: Double): Point =
    Point(Latitude.unsafeMake(latDeg.degrees), Longitude.unsafeMake(lonDeg.degrees))

  test("Delaunay triangulation with 3 non-collinear points should produce one triangle") {
    val p1        = pt(0, 0)
    val p2        = pt(0, 1)
    val p3        = pt(1, 0)
    val points    = List(p1, p2, p3)
    val triangles = TrianglesServices.Delaunay(points)

    assertEquals(triangles.size, 1, "Should produce one triangle")
    // Check if the triangle contains the correct points (order might vary)
    val expectedTrianglePoints = Set(p1, p2, p3)
    val actualTrianglePoints   = Set(triangles.head.p1, triangles.head.p2, triangles.head.p3)
    assertEquals(actualTrianglePoints, expectedTrianglePoints, "Triangle vertices do not match")
  }

  test("Delaunay triangulation with 4 points forming a convex quadrilateral should produce two triangles") {
    val p1        = pt(0, 0) // A
    val p2        = pt(0, 2) // B
    val p3        = pt(2, 2) // C
    val p4        = pt(2, 0) // D
    val points    = List(p1, p2, p3, p4)
    val triangles = TrianglesServices.Delaunay(points)

    assertEquals(triangles.size, 2, "Should produce two triangles for a convex quadrilateral")
    // Further checks could involve ensuring the two triangles share an edge and cover the quad
    // For simplicity, we'll just check the number of triangles for now.
    // A more robust check would verify the specific triangles, e.g., (p1,p2,p3) and (p1,p3,p4) or (p1,p2,p4) and (p2,p3,p4)
  }

  test("Delaunay with 0 points should throw RuntimeException") {
    intercept[RuntimeException] {
      TrianglesServices.Delaunay(List.empty)
    }
  }

  test("Delaunay with 1 point should throw RuntimeException") {
    intercept[RuntimeException] {
      TrianglesServices.Delaunay(List(pt(0, 0)))
    }
  }

  test("Delaunay with 2 points should throw RuntimeException") {
    intercept[RuntimeException] {
      TrianglesServices.Delaunay(List(pt(0, 0), pt(1, 1)))
    }
  }

  test(
    "Delaunay with 3 collinear points should throw RuntimeException (or return empty list depending on Tinfour behavior)"
  ) {
    val p1     = pt(0, 0)
    val p2     = pt(1, 1)
    val p3     = pt(2, 2)
    val points = List(p1, p2, p3)

    // Tinfour's IncrementalTin might not bootstrap with only collinear points.
    // If it bootstraps but forms no triangles, it would return empty.
    // If it fails to bootstrap, it throws an exception as per the code.
    // Let's assume it throws for now, based on the explicit check.
    intercept[RuntimeException] {
      TrianglesServices.Delaunay(points)
    }
    // Alternatively, if Tinfour handles this by returning no triangles:
    // val triangles = TrianglesServices.Delaunay(points)
    // assertEquals(triangles.size, 0, "Should produce no triangles for 3 collinear points if TIN bootstraps")
  }

  test("Delaunay with 4 collinear points should throw RuntimeException") {
    val p1     = pt(0, 0)
    val p2     = pt(1, 1)
    val p3     = pt(2, 2)
    val p4     = pt(3, 3)
    val points = List(p1, p2, p3, p4)
    intercept[RuntimeException] {
      TrianglesServices.Delaunay(points)
    }
  }

  // A slightly more complex case
  test("Delaunay with 5 points") {
    val p1        = pt(0, 0)
    val p2        = pt(0, 4)
    val p3        = pt(4, 4)
    val p4        = pt(4, 0)
    val p5        = pt(2, 2) // Center point
    val points    = List(p1, p2, p3, p4, p5)
    val triangles = TrianglesServices.Delaunay(points)
    // Expected: 4 triangles meeting at the center point p5
    assertEquals(triangles.size, 4, "Should produce 4 triangles for a square with a center point")

    // Verify all points are used and p5 is part of all triangles
    val allVerticesInTriangles = triangles.flatMap(t => List(t.p1, t.p2, t.p3)).toSet
    assertEquals(allVerticesInTriangles, points.toSet, "All input points should be vertices of some triangle")
    assert(
      triangles.forall(t => List(t.p1, t.p2, t.p3).contains(p5)),
      "Center point p5 should be part of all triangles"
    )
  }
}
