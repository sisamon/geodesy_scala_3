
package com.a3.capex.geodesy

import Coordinates._
import CoordinatesImplicits.{
 //  given_Conversion_Longitude_, given_Conversion_Latitude_Double,
 given_Conversion_Longitude_Angle, given_Conversion_Latitude_Angle,
 given_Conversion_Angle_Longitude, given_Conversion_Angle_Latitude,
 given_Conversion_Angle_Double
 // ,LatitudeOrder, LongitudeOrder
}
import com.a3.capex.geodesy.ShapesCore._

import squants._
import squants.space.{Angle, Degrees, Kilometers, SquareMeters}
import squants.space.AngleConversions._
import squants.space.LengthConversions.LengthNumeric
import java.lang.Math._
import scala.annotation.tailrec
import scala.math.sqrt
import neotype._

import org.tinfour.*
import org.tinfour.common.*
import org.tinfour.standard.*
import scala.jdk.CollectionConverters.*
import org.tinfour.standard.IncrementalTin
import org.tinfour.common.Vertex
import scala.jdk.CollectionConverters._
//
//object TrianglesServices:
//  def Delaunay(vertexes: List[Point]): List[TRriangles] =
