package org.a3.capex


import squants.space.{Angle, Degrees, Kilometers, SquareMeters}
import squants.space.AngleConversions.*
import com.a3.geodesy.Coordinates.{Latitude, Longitude}
import com.a3.geodesy.CoordinatesImplicits.{
  given_Conversion_Angle_Latitude, given_Conversion_Angle_Longitude,
  given_Conversion_Double_Angle, given_Conversion_Longitude_Angle
}  // given_Conversion_Angle_Double,

@main
def main(): Unit = 

  println("Hello world!")
  val d1: Double = 45.0
  val d2: Double = 18.5
  val a1: Angle = d1
  val a2: Angle = d2
  val l1: Longitude = a1 // Longitude.unsafeMake(d1.degrees)
  val l2: Longitude = Longitude.unsafeMake(d2.degrees)

  val l: Longitude = l1 + l2
  // It would require given_Conversion_Angle_Double
  // but then I get into an infinite loop with given_Conversion_Double_Angle
  // Luckily this is not a use case we will face often, I think.
  // val d: Double = a1

  println(l)

