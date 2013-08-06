package Spray1

import scala.concurrent.duration._
import akka.pattern.ask
import akka.util.Timeout
import akka.actor._
import spray.can.Http
import spray.can.server.Stats
import spray.http._
import HttpMethods._
import MediaTypes._
import java.io._
import java.net.URL
import javax.swing.text.AbstractDocument.Content
import scala.collection.mutable
import spray.json.{JsonParser, DefaultJsonProtocol}
import DefaultJsonProtocol._
import SprayTest.{MyJsonProtocol, Person}
import scala.util.parsing.json.{JSONArray, JSONObject}
import spray.httpx.marshalling._
import spray.httpx.unmarshalling.{Unmarshaller, pimpHttpEntity}
import spray.util._
import spray.http._



class DemoService extends Actor with SprayActorLogging with DefaultJsonProtocol {
  implicit val timeout: Timeout = 1.second // for the actor 'asks'
  import context.dispatcher // ExecutionContext for the futures and scheduler

/*
  implicit val personFormat = jsonFormat4(Person)


  implicit val PersonUnmarshaller =
    Unmarshaller[Person] {
      case HttpBody(contentType, buffer) =>
        // unmarshal from the string format used in the marshaller example
        val Array(name, age, sex, address) =
          buffer.asString.split(":,".toCharArray).map(_.trim)
        Person(name, age.toInt, sex, address)
    }

*/


  def appendFile(fileName: String, line: String) = {
    val fw = new FileWriter(fileName , true) ;
    fw.write( line + "\n") ;
    fw.close()
  }

/*
  def finder (_ , name :String , _, age : Int , _, sex , _, address) = {
      var string ="{\"name\" : \""
      if (name == Nil)
        string = string + " \", \"age\" : "
      else
        string = string + name + "\", \"age\" : "
      if (age == Nil)
        string = string + "-1,  \"sex\" : \""
      else
        string = string + age + ",  \"sex\" : \""
      if (sex == Nil)
        string = string + " \" , \"address\" : \""
      else
        string = string + sex + "\" , \"address\" : \""
      if (address == Nil)
        string = string + " \"} "
      else
        string = string + address + "\"} "

      println(string)


  }
*/



  def receive = {
    // when a new connection comes in we register ourselves as the connection handler
    case _: Http.Connected => sender ! Http.Register(self)

    case HttpRequest(GET, Uri.Path("/"), _, _, _) =>
      sender ! index

    case HttpRequest(GET, Uri.Path("/plik"), _, _, _) =>
      sender ! fileOperations


    case HttpRequest(GET, Uri.Path("/open"), _, _, _)  =>
      val source = scala.io.Source.fromFile("file.txt")
      val lines = source.mkString
      sender ! HttpResponse(entity = lines)
      source.close()


    case HttpRequest(GET, Uri.Path("/addingName"), _, _ , _) =>
      sender ! FormAdding


    case HttpRequest(GET, Uri.Path("/removeName"), _, _ , _) =>
      sender ! FormRemove

    case HttpRequest(GET, Uri.Path("/findBy"), _, _ , _) =>
      sender ! FormFind


    case HttpRequest(POST, Uri.Path("/append"),_ , test, _) =>
      var data = test.toString
      data = data.substring(data.indexOf(',')+1 , data.length -1  )
      val Array(_ , name : String , _, age : String , _, sex : String, _, address : String)  =
        data.split("=&".toCharArray)
      val string = "{\"name\" : \"%s\", \"age\" : %s,  \"sex\" : \"%s\" , \"address\" : \"%s\"} ".format(name , age, sex, address)
    //  val array  =  data.split("=&".toCharArray).map(_.trim)
     // val p = marshal(Person(array(1).toString, array(3).toInt, array(5).toString, array(7).toString))

    //println(p)


    /*    var content = test.asString
        content = "{ \"" + content.replace("=","\" : \"") + "\" }"
        content = "\"\"\" " + content.replace("&","\" , \"") + "\"\"\""
        println(content)
        val json = JsonParser(content).convertTo[Person]
        println(content)*/
      appendFile("file.txt", string)
      val source = scala.io.Source.fromFile("file.txt")
      val lines = source.mkString
      sender ! HttpResponse(entity = lines)
      source.close()

    case HttpRequest(POST, Uri.Path("/remove"),_ , test, _) =>
      val nameToRemove = test.asString.substring(5)
      var map = new mutable.HashMap[String, String]()

      import MyJsonProtocol._

      try{
        var source = scala.io.Source.fromFile("file.txt")
        for ( line <- source.getLines()){
          println(JsonParser(line).convertTo[Person].name)
          if (JsonParser(line).convertTo[Person].name != nameToRemove)
            map(line) = line
        }
        source.close()
        val pw = new java.io.PrintWriter(new File("file.txt"))


        for (line <- map.iterator){
          //appendFile("file.txt", line._2)
          pw.write(line._2 + "\n")
        }
        pw.close()
        source = scala.io.Source.fromFile("file.txt")
        val lines = source.mkString
        sender ! HttpResponse(entity = lines)

        source.close()
      }




    case HttpRequest(POST, Uri.Path("/find"),_ , test, _) =>
      val valueToFind = test.asString.substring(5)
      //println("test " + test.toString)

      var data = test.toString
      data = data.substring(data.indexOf(',')+1 , data.length -1  )
      val array = data.split("&=".toCharArray)

      println("data " + array.mkString(" "))
      var string ="{\"name\" : \""


      if (array(1) == ""){
        string = string + " \", \"age\" : "
      }
      else
        string = string + array(1) + "\", \"age\" : "

     println("string po 1: " + string)

      if (array(3) == "")
        string = string + "-1,  \"sex\" : \""
      else
        string = string + array(3) + ",  \"sex\" : \""

      if (array(5) == "")
        string = string + "\" , \"address\" : \""
      else
        string = string + array(5) + "\" , \"address\" : \""

      if (array.length == 8)
        string = string + array(8) + "\"} "
      else
        string = string + "\"} "

     println(array.length)
    println( string)


 /*     val Array(_ , name : String , _, age : String , _, sex : String, _, address : String)  =
        data.split("&=".toCharArray)
      val str = "{\"name\" : \"%s\", \"age\" : %s,  \"sex\" : \"%s\" , \"address\" : \"%s\"} ".format(name , age, sex, address)





      //val json = JsonParser(str)
      println("json" + str)*/
      import MyJsonProtocol._
      personToFind = JsonParser(string).convertTo[Person]
      try{
        var source = scala.io.Source.fromFile("file.txt")
        var string  = ""
        for ( line <- source.getLines()){
          //println(JsonParser(line).convertTo[Person].name)
          var jsonLine =  JsonParser(line).convertTo[Person]
          if (JsonParser(line).convertTo[Person].sex == valueToFind)
             string = string  + line + "\n"
        }

        sender ! HttpResponse(entity = string)

        source.close()
      }




    /******************************************************************************/
    case HttpRequest(GET, Uri.Path("/stream"), _, _, _) =>
      val peer = sender // since the Props creator is executed asyncly we need to save the sender ref
      context actorOf Props(new Streamer(peer, 25))

    case HttpRequest(GET, Uri.Path("/server-stats"), _, _, _) =>
      val client = sender
      context.actorFor("/user/IO-HTTP/listener-0") ? Http.GetStats onSuccess {
        case x: Stats => client ! statsPresentation(x)
      }

    case HttpRequest(GET, Uri.Path("/crash"), _, _, _) =>
      sender ! HttpResponse(entity = "About to throw an exception in the request handling actor, " +
        "which triggers an actor restart")
      sys.error("BOOM!")

    case HttpRequest(GET, Uri.Path(path), _, _, _) if path startsWith "/timeout" =>
      log.info("Dropping request, triggering a timeout")

    case HttpRequest(GET, Uri.Path("/stop"), _, _, _) =>
      sender ! HttpResponse(entity = "Shutting down in 1 second ...")
      context.system.scheduler.scheduleOnce(1.second) { context.system.shutdown() }

    case _: HttpRequest => sender ! HttpResponse(status = 404, entity = "Unknown resource!")

    case Timedout(HttpRequest(_, Uri.Path("/timeout/timeout"), _, _, _)) =>
      log.info("Dropping Timeout message")

    case Timedout(HttpRequest(method, uri, _, _, _)) =>
      sender ! HttpResponse(
        status = 500,
        entity = "The " + method + " request to '" + uri + "' has timed out..."
      )

  }



  ////////////// helpers //////////////

  lazy val index = HttpResponse(
    entity = HttpEntity(`text/html`,
      <html>
        <body>
          <h1>Say hello to <i>spray-can</i>!</h1>
          <p>Defined resources:</p>
          <ul>
            <li><a href="/plik">/Plik</a></li>
            <li><a href="/stream">/stream</a></li>
            <li><a href="/server-stats">/server-stats</a></li>
            <li><a href="/crash">/crash</a></li>
            <li><a href="/timeout">/timeout</a></li>
            <li><a href="/timeout/timeout">/timeout/timeout</a></li>
            <li><a href="/stop">/stop</a></li>
          </ul>
        </body>
      </html>.toString()
    )
  )

  def statsPresentation(s: Stats) = HttpResponse(
    entity = HttpEntity(`text/html`,
      <html>
        <body>
          <h1>HttpServer Stats</h1>
          <table>
            <tr><td>uptime:</td><td>{s.uptime.formatHMS}</td></tr>
            <tr><td>totalRequests:</td><td>{s.totalRequests}</td></tr>
            <tr><td>openRequests:</td><td>{s.openRequests}</td></tr>
            <tr><td>maxOpenRequests:</td><td>{s.maxOpenRequests}</td></tr>
            <tr><td>totalConnections:</td><td>{s.totalConnections}</td></tr>
            <tr><td>openConnections:</td><td>{s.openConnections}</td></tr>
            <tr><td>maxOpenConnections:</td><td>{s.maxOpenConnections}</td></tr>
            <tr><td>requestTimeouts:</td><td>{s.requestTimeouts}</td></tr>
          </table>
        </body>
      </html>.toString()
    )
  )


  lazy val fileOperations = HttpResponse(
    entity = HttpEntity(`text/html`,
      <html>
        <body>
          <h1>Say hello to <i>spray-can</i>!</h1>
          <p>Defined resources:</p>
          <ul>
            <li><a href="/open">/Wyswietl_plik</a></li>
            <li><a href="/addingName">/Add Name</a></li>
            <li><a href="/findBy">/Find by</a></li>
            <li><a href="/removeName">/Remove Name</a></li>
          </ul>
        </body>
      </html>.toString()
    )
  )

  lazy val FormAdding = HttpResponse (
  entity = HttpEntity(`text/html`,
  <html>
    <head>
        <link rel="stylesheet" type="text/css" href="mystyle.css" ></link>
    </head>
    <body>
      <h1>Add to file</h1>
        <form name="input" action="/append" method="post">
          <div id ="formWrapper">
            <label for="firstname">First name</label>
            <input type ="text" placeholder="First name" name="firstname"></input>
            <br/>

            <label for="age">Age</label>
            <input type ="text" placeholder="Age" name="age" ></input>
            <br/>

            <label for="sex">Sex</label>
            <input type ="text" placeholder="Male" name="sex" ></input>
            <br/>

            <label for="address">Address</label>
            <input type ="text" placeholder="Address" name="address" ></input>
            <br/>

           <input type="submit" value="Submit"></input>

           <br/>

          </div>
        </form>
      </body>
  </html>.toString
  )
  )


  lazy val FormRemove = HttpResponse (
    entity = HttpEntity(`text/html`,
      <html>
        <body>
          <h1>Remove from file</h1>
          <form name="input" action="/remove" method="post" />
          Username: <input type="text" name="user" />
          <input type="submit" value="Submit" />
        </body>
      </html>.toString()
    )
  )

  lazy val FormFind = HttpResponse (
    entity = HttpEntity(`text/html`,
      <html>
        <body>
          <h1>Find by </h1>
          <form name="input" action="/find" method="post" />
          Name: <input type="text" name="name" /> <br/>
          Age: <input type="text" name="age" /> <br/>
          Sex: <input type="text" name="sex" /> <br/>
          Address: <input type="text" name="address" /> <br/>
          <input type="submit" value="Submit" />
          <br/>
        </body>
      </html>.toString()
    )
  )

  class Streamer(client: ActorRef, count: Int) extends Actor with SprayActorLogging {
    log.debug("Starting streaming response ...")

    // we use the successful sending of a chunk as trigger for scheduling the next chunk
    client ! ChunkedResponseStart(HttpResponse(entity = " " * 2048)).withAck(Ok(count))

    def receive = {
      case Ok(0) =>
        log.info("Finalizing response stream ...")
        client ! MessageChunk("\nStopped...")
        client ! ChunkedMessageEnd
        context.stop(self)

      case Ok(remaining) =>
        log.info("Sending response chunk ...")
        context.system.scheduler.scheduleOnce(100 millis span) {
          client ! MessageChunk(DateTime.now.toIsoDateTimeString + ", ").withAck(Ok(remaining - 1))
        }

      case x: Http.ConnectionClosed =>
        log.info("Canceling response stream due to {} ...", x)
        context.stop(self)
    }

    // simple case class whose instances we use as send confirmation message for streaming chunks
    case class Ok(remaining: Int)
  }
}