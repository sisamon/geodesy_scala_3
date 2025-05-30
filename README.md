I want to use this for two things main things:
- Test different approaches squants + neotype + Scalaz vs ZIO Prelude
- Share a solution for what is probably a very small need, being able to use "points" as keys on maps while maintining good ergonomy.

First commit built using squants + neotype + Scalaz it builds and runs.
That is all.




The sbt publishLocal command completed successfully!

The library com.a3.capex:geodesy_3:0.1.0-SNAPSHOT is now published to your local Ivy repository at /Users/luis/.ivy2/local/. You can now add it as a dependency in other sbt projects using:

scala
CopyInsert
libraryDependencies += "com.a3.capex" %% "geodesy" % "0.1.0-SNAPSHOT"
(Note the use of %% which automatically appends the Scala version, and assuming your artifact name in build.sbt is geodesy. If it's geodesy_3 as suggested by the output, then it would be libraryDependencies += "com.a3.capex" % "geodesy_3" % "0.1.0-SNAPSHOT" or libraryDependencies += "com.a3.capex" %% "geodesy" % "0.1.0-SNAPSHOT" if the name setting in your build.sbt is just geodesy and sbt appends _3 automatically for Scala 3.)

I did notice a warning in the output:

CopyInsert
[warn] there's a key that's not used by any other settings/tasks:
[warn]  
[warn] * geodesy_scala_3 / idePackagePrefix
[warn]   +- /Volumes/Workspace/PROJECTS/bedtests/geodesy/geodesy_scala_3/build.sbt:25
This idePackagePrefix warning is usually related to IDE integration (like IntelliJ IDEA or Metals for VS Code) and suggests that a setting intended to help the IDE organize source files or packages might not be correctly configured or is simply unused. It generally doesn't affect the functionality of the published library itself but could be cleaned up for good practice.

Would you like to investigate and fix this idePackagePrefix warning, or are you ready to move on?

