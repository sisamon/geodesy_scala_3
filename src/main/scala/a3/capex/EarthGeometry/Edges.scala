package org.a3.geodesy

import com.a3.geodesy.Coordinates._
import com.a3.geodesy.CoordinatesImplicits.{given_Conversion_Longitude_Angle, given_Conversion_Latitude_Angle}

import com.a3.geodesy.ShapesCore._

import squants._
import squants.space.{Angle, Degrees, Kilometers, SquareMeters}
import squants.space.AngleConversions._
import squants.space.LengthConversions.LengthNumeric
import java.lang.Math._
import scala.annotation.tailrec
import scala.math.sqrt
import scalaz._
import scalaz.Scalaz._
import neotype._


object Edges:

  case class Edge(origin: Point, end: Point) extends CurvedShapes:
    override def barycenter = Point(Latitude.unsafeMake((origin.latitude + end.latitude) / 2), Longitude.unsafeMake((origin.longitude + end.longitude) / 2))
    override def north = origin.latitude
    override def south = end.latitude
    override def east = origin.longitude
    override def west = end.longitude

    // An edge can not contain anything.
    override def contains(geom: Box) = false

    override def toString: String = s"($origin.latitude , $origin.longitude  -> $end.latitude , $end.longitude )"

    // Sorted by latitude, used to ensure membership of edges
    def sorted(): Edge =
      if (origin.isSorted(end)) Edge(origin, end) else Edge(end, origin)


  object Edge:
    implicit val equal: Equal[Edge] = Equal.equalA // [Edge]

    given Conversion[Edge, Box] = _.toBox
