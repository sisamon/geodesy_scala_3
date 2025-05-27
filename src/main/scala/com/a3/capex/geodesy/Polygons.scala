//// This was commented.
//case class Triangulation(input: DataInput, partitionLevel: PartitionLevel) {
//  // First I purge those sites too close to each other, no further criteria used
//  val torrerosUnicos: Set[Point.PointKey] = Triangulation.cobertura(input.torreros map { s => s.coordinates }, Params.proximidad)
//
//  // And now I compute the distance to the Telefonica sites
//  val torrerosDistance: Set[Point.PointKey] = (torrerosUnicos map { s =>
//    (s, input.telefonicaNetworkPoints
//      .withFilter(x => s.toPoint.distance(x) < Params.proximidad)
//      .map { p => p -> s.toPoint.distance(p) }.groupBy(_._1))
//  }).filter(_._2.isEmpty) map {
//    _._1
//  }
//
//  // And now we build the points for the triangulation
//  val puntosTriangulacion: Array[Point.PointKey] = input.telefonicaFullReference.keys.toSet.union(torrerosDistance).toArray
//
//  // And turn them into a grid
//  val puntosTriangulacionGrid: Map[Point.PointGrid, Set[Point.PointKey]] = (puntosTriangulacion
//    map { x => (x.gridCoordinates, x) }).groupBy(_._1)
//    .map { case (k, v) => (k, (v map { case (_, k2) => k2 }).toSet) }
//
//  val candidates: List[TrianglePreAllocation] = Triangulation.candidates(puntosTriangulacion,
//    puntosTriangulacionGrid, input.pointToDiameterCurrent, input.anyPointToRegion, input.anyPointToTerrain, input.heightFromPoint, input.imported.radios, true)
//
//  // And finally we partition the triangles
//  val trianglePartition: List[TrianglePreAllocation] = candidates flatMap {
//    _.partition(partitionLevel.levels)
//  }
//
//
//  // Esto es para escribir los datos en hive. Asociamos enteros con los triangulos y los puntos.
//  val triangulosOutdoor: List[Triangle] = trianglePartition.map(_.t)
//
//  // Aqui tenemos los indoor. Incluimos los indoor GSM porque podemos usarlos para actuar.
//  val coordenadasIndoor: Array[Point.PointKey] = (input.telefonicaIndoorRadioToday map {
//    case (k, _) => k.p
//  }).toArray
//
//  val triangulosIndoor: List[Triangle] = input.currentIndoor.map { x =>
//    val p = x.coordinates
//    Triangle(p, p, p)
//  }.distinct
//
//  // This is an indexed triangle map
//  val trianglePartitionMap: Map[Triangle, Int] = (triangulosOutdoor ++ triangulosIndoor).
//    zipWithIndex.
//    map { x => (x._1, x._2) }.
//    toMap
//
//  val allPoints: List[(Point.PointKey, TipoInstalacion)] =
//    puntosTriangulacion.toList.strengthR(TipoInstalacion.OutdoorInstallation) :::
//      coordenadasIndoor.toList.strengthR(TipoInstalacion.IndoorInstallation)
//
//  // And this is a indexed location map that now includes indoor
//  val puntosTriangulacionMap: Map[Point.PointKey, Int] = allPoints.map(_._1).zipWithIndex.toMap
//}
//
//object Triangulation {
//  def cobertura(puntos: List[Point], d: Length): Set[Point.PointKey] = {
//    val distancias: Map[Point.PointKey, List[Point]] = puntos
//      .map { s => (s.keyCoordinates, puntos.filter(x => x != s && x.distance(s) < d)) }.toMap
//    val (iniciales, candidatos) = distancias.partition(_._2.isEmpty)
//
//    @tailrec def addPoint(coverSet: Set[Point.PointKey], candidatos: List[Point.PointKey]): Set[Point.PointKey] = {
//      candidatos match {
//        case head :: rest => addPoint(coverSet + head, rest.filterNot(distancias.getOrElse(head, Nil).contains))
//        case Nil => coverSet
//      }
//    }
//
//    addPoint(iniciales.keys.toSet, candidatos.keys.toList)
//  }
//
//  // Calculate the candidates out of the triangles
//  def candidates(puntosTriangulacion: Array[Point.PointKey],
//                 puntosTriangulacionGrid: Map[Point.PointGrid, Set[Point.PointKey]],
//                 pointToDiameterCurrent: Map[Point.PointKey, Diameter],
//                 anyPointToRegion: Map[Point.PointKey, Region],
//                 anyPointToTerrain: Map[Point.PointKey, TipoZona],
//                 heightFromPoint: (Point.PointKey, Diameter) => Length,
//                 radios: Map[RadioKey, Length],
//                 verbose: Boolean): List[TrianglePreAllocation] =
//    meshBuilder(puntosTriangulacion,
//      puntosTriangulacionGrid, pointToDiameterCurrent, anyPointToRegion,
//      anyPointToTerrain, heightFromPoint, radios, verbose)
//
//}
