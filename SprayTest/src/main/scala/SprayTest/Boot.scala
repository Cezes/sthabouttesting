import akka.actor.{ActorSystem, Props}
import akka.io.IO
import spray.can.Http
import Spray1.DemoService



import akka.actor.{ActorSystem, Props}
import akka.io.IO
import spray.can.Http

object Boot extends App {

  // we need an ActorSystem to host our application in
  implicit val system = ActorSystem("on-spray-can")

  // create and start our service actor
  val service = system.actorOf(Props[Spray1.DemoServiceActor], "demo-service")

  // start a new HTTP server on port 8080 with our service actor as the handler
  IO(Http) ! Http.Bind(service, "localhost", port = 8080)
}
/*
object Main extends App with MySslConfiguration {

  implicit val system = ActorSystem()

  // the handler actor replies to incoming HttpRequests
  val handler = system.actorOf(Props[DemoService], name = "handler")

  IO(Http) ! Http.Bind(handler, interface = "localhost", port = 8080)
}*/
