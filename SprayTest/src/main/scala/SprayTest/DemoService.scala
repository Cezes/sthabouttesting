package Spray1

/*import scala.concurrent.duration._
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
import scalax.io._*/

import java.io.File
import org.parboiled.common.FileUtils
import scala.concurrent.duration._
import akka.actor.{Props, Actor}
import akka.pattern.ask
import spray.routing.{HttpService, RequestContext}
import spray.routing.directives.CachingDirectives
import spray.can.server.Stats
import spray.can.Http
import spray.httpx.marshalling.Marshaller
import spray.httpx.encoding.Gzip
import spray.util._
import spray.http._
import MediaTypes._
import spray.routing.directives.CachingDirectives._
import spray.http.HttpHeaders.RawHeader
import spray.routing._
import scala.concurrent.Future

// we don't implement our route structure directly in the service actor because
// we want to be able to test it independently, without having to spin up an actor
class DemoServiceActor extends Actor with DemoService {

  // the HttpService trait defines only one abstract member, which
  // connects the services environment to the enclosing actor or test
  def actorRefFactory = context

  // this actor only runs our route, but you could add
  // other things here, like request stream processing,
  // timeout handling or alternative handler registration
  def receive = runRoute(demoRoute)
}

// this trait defines our service behavior independently from the service actor
trait DemoService extends HttpService{

  // we use the enclosing ActorContext's or ActorSystem's dispatcher for our Futures and Scheduler
  implicit def executionContext = actorRefFactory.dispatcher

  val demoRoute = {
    handleRejections(myHandler) {
      allowCrossDomain {
    get{
      path("") {
        complete(index)
      }~
      pathPrefix("plik") {
          path("open"){
            val source = scala.io.Source.fromFile("file.txt")
            val lines = source.mkString
            source.close()
            complete(lines)
          }~
          path("findBy"){
           // println(FormFind.text)
           // println(FormFind.toList.mkString(" "))
           respondWithMediaType(`text/html`)(complete(FormFind))
          }~
            path(""){
            complete(fileOperations)
          }
      }~
       path("stats") {
          complete {
            actorRefFactory.actorFor("/user/IO-HTTP/listener-0")
              .ask(Http.GetStats)(1.second)
              .mapTo[Stats]
          }
        } ~
        path("timeout") { ctx =>
          // we simply let the request drop to provoke a timeout
        } ~
        path("crash") { ctx =>
          sys.error("crash boom bang")
        } ~
        path("fail") {
          failWith(new RuntimeException("aaaahhh"))
        }
    } ~
      (post | parameter('method ! "post")) {
        path("stop") {
          complete {
            in(1.second){ actorSystem.shutdown() }
            "Shutting down in 1 second..."
          }
        }~
          path("find"){
          complete("Pong !")
        }~
        path("addingName"){
          complete(FormAdding)
        }~
        path("append"){
          complete("pong !")
        }
      } ~
     pathPrefix("api") {
      path("api-docs") {
        val source = scala.io.Source.fromFile("api-docs.json")
        val lines = source.mkString
        source.close()
        complete(lines)

      }
    }
  }
 }
}
  //lazy val simpleRouteCache = routeCache(maxCapacity = 1000, timeToIdle = Duration("30 min"))

  lazy val index =
    <html xmlns="http://www.w3.org/1999/xhtml" lang="pl" xml:lang="pl" >
      <body>
        <h1>Say hello to <i>spray-routing</i> on <i>spray-can</i>!</h1>
        <p>Defined resources:</p>
        <ul>
          <li><a href="/plik">/plik</a></li>
          <li><a href="/stats">/stats</a></li>
          <li><a href="/timeout">/timeout</a></li>
          <li><a href="/crash">/crash</a></li>
          <li><a href="/fail">/fail</a></li>
          <li><a href="/stop?method=post">/stop</a></li>
        </ul>
      </body>
    </html>


  lazy val fileOperations =
    <html xmlns="http://www.w3.org/1999/xhtml" lang="pl" xml:lang="pl" >
        <body>
          <h1>Say hello to <i>spray-can</i>!</h1>
          <p>Defined operations:</p>
          <ul>
            <li><a href="/plik/open">/Display file</a></li>
            <li><a href="/plik/addingName">/Add record</a></li>
            <li><a href="/plik/findBy">/Find by</a></li>
            <li><a href="/plik/edit">/Edit record</a></li>
            <li><a href="/plik/removeName">/Remove record</a></li>
          </ul>
        </body>
      </html>


  lazy val FormAdding =
      <html xmlns="http://www.w3.org/1999/xhtml" lang="pl" xml:lang="pl" >
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
      </html>


  lazy val FormRemove =
      <html xmlns="http://www.w3.org/1999/xhtml" lang="pl" xml:lang="pl" >
        <body>
          <h1>Remove from file</h1>
          <form name="input" action="/remove" method="post" />
          Username: <input type="text" name="user" />
          <input type="submit" value="Submit" />
        </body>
      </html>


  lazy val FormFind =
      <html xmlns="http://www.w3.org/1999/xhtml" lang="pl" xml:lang="pl" >
        <body>
          <h1>Find by </h1>
          <form name="input" action="/find" method="post" />
          Name: <input type="text" name="name" /> <br/>
          Age: <input type="text" name="age" /> <br/>
          Sex: <input type="text" name="sex" /> <br/>
          Address: <input type="text" name="address" /> <br/>
          <input type="submit" value="Find" />
          <br/>
        </body>
      </html>

  lazy val FormEdit =
      <html xmlns="http://www.w3.org/1999/xhtml" lang="pl" xml:lang="pl" >
        <body>
          <h1>Find record you want to edit </h1>
          <form name="input" action="/edycja" method="post" />
          Name: <input type="text" name="name" /> <br/>
          Age: <input type="text" name="age" /> <br/>
          Sex: <input type="text" name="sex" /> <br/>
          Address: <input type="text" name="address" /> <br/>
          <input type="submit" value="Find" />
          <br/>
        </body>
      </html>



  implicit val statsMarshaller: Marshaller[Stats] =
    Marshaller.delegate[Stats, String](ContentTypes.`text/plain`) { stats =>
      "Uptime                : " + stats.uptime.formatHMS + '\n' +
        "Total requests        : " + stats.totalRequests + '\n' +
        "Open requests         : " + stats.openRequests + '\n' +
        "Max open requests     : " + stats.maxOpenRequests + '\n' +
        "Total connections     : " + stats.totalConnections + '\n' +
        "Open connections      : " + stats.openConnections + '\n' +
        "Max open connections  : " + stats.maxOpenConnections + '\n' +
        "Requests timed out    : " + stats.requestTimeouts + '\n'
    }


  def in[U](duration: FiniteDuration)(body: => U): Unit =
    actorSystem.scheduler.scheduleOnce(duration)(body)
  def oldRh = implicitly[RejectionHandler]

  def myHandler = RejectionHandler.apply {
    case x => allowCrossDomain { oldRh(x) }
  }

  def allowCrossDomain =
    respondWithHeaders(
      RawHeader("Access-Control-Allow-Origin", "*"),
      RawHeader("Access-Control-Allow-Headers", "Content-Type"),
      RawHeader("Access-Control-Allow-Methods", "GET, PUT, POST"))


}

/*

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


  def parseToFindPerson ( array: Array[String] ) : String = {
    var jsonPerson ="{\"name\" : \""
    println(array.getClass)

    if (array(1) == ""){
      jsonPerson = jsonPerson + " \", \"age\" : "
    }
    else
      jsonPerson = jsonPerson + array(1) + "\", \"age\" : "

    println("string po 1: " + jsonPerson)

    if (array(3) == "")
      jsonPerson = jsonPerson + "-1,  \"sex\" : \""
    else
      jsonPerson = jsonPerson + array(3) + ",  \"sex\" : \""

    if (array(5) == "")
      jsonPerson = jsonPerson + "\" , \"address\" : \""
    else
      jsonPerson = jsonPerson + array(5) + "\" , \"address\" : \""

    if (array.length == 8)
      jsonPerson = jsonPerson + array(7) + "\"} "
    else
      jsonPerson = jsonPerson + "\"} "

    jsonPerson
  }


  def findMatch(line : String ,  personToFind : Person ) : String = {
    import MyJsonProtocol._
    var currentLine =  JsonParser(line).convertTo[Person]
    var currentLineResult =  line + "\n"

    //var string =""
    if (personToFind.name == " " ){
    }
    else if (currentLine.name != personToFind.name)
      currentLineResult =""
    if (personToFind.age ==  -1 ) {
    }
    else if(currentLine.age != personToFind.age)
      currentLineResult =""
    if (personToFind.sex == ""  ){
    }
    else if (currentLine.sex != personToFind.sex)
      currentLineResult = ""
    if (personToFind.address == ""  ){
    }
    else if (currentLine.address != personToFind.address)
      currentLineResult =""

    currentLineResult

  }



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

    case HttpRequest(GET, Uri.Path("/edit"), _, _, _) =>
      println("xxx")
      sender ! FormEdit


    case HttpRequest(POST, Uri.Path("/append"),_ , test, _) =>
      var data = test.toString
      data = data.substring(data.indexOf(',')+1 , data.length -1  )
      val Array(_ , name : String , _, age : String , _, sex : String, _, address : String)  =
        data.split("=&".toCharArray)
      val jsonToAppend = "{\"name\" : \"%s\", \"age\" : %s,  \"sex\" : \"%s\" , \"address\" : \"%s\"} ".format(name , age, sex, address)
      appendFile("file.txt", jsonToAppend)
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

        source = scala.io.Source.fromFile("file.txt")
        val lines = source.mkString
        sender ! HttpResponse(entity = lines)

        source.close()
      }




    case HttpRequest(POST, Uri.Path("/find"),  _ , test, _) =>
/*      val entity = test.getEntity
      var content =""
      if (entity != null) {
        val inputStream = entity.getContent
        content = io.Source.fromInputStream(inputStream).getLines.mkString
        inputStream.close
      }*/


      var data = test.toString
      println("test" + test.buffer.mkString(" "))
      println(data)
      println(Uri.toString())
      data = data.substring(data.indexOf(',')+1 , data.length -1  )
      val array = data.split("&=".toCharArray)

      val jsonPerson = parseToFindPerson(array)

      import MyJsonProtocol._
      val personToFind = JsonParser(jsonPerson).convertTo[Person]
      println("Person we are looking for: " + personToFind)
      try{
        var source = scala.io.Source.fromFile("file.txt")
        var result = ""
        for ( line <- source.getLines()){
          var currentLineResult = findMatch( line, personToFind)
          result = result + currentLineResult
          currentLineResult =""
        }

        sender ! HttpResponse(entity = result)

        source.close()
      }

    case HttpRequest(POST, Uri.Path("/edycja"), _, test, _) =>

      var data = test.toString
      data = data.substring(data.indexOf(',')+1 , data.length -1  )
      val array = data.split("&=".toCharArray)

      val jsonPerson = parseToFindPerson(array)

      import MyJsonProtocol._

      var result =""
      val personToFind = JsonParser(jsonPerson).convertTo[Person]
      println("Person we are looking for: " + personToFind)

      try{
        var source = scala.io.Source.fromFile("file.txt")

        for ( line <- source.getLines()){
          var currentLineResult = findMatch( line, personToFind)
          if (currentLineResult == "")
            result = result + line + "\n"
          currentLineResult =""
        }
       source.close()

        val pw = new java.io.PrintWriter(new File("file.txt"))

        pw.write(result)

        pw.close()



        //sender ! HttpResponse(entity = result)


      }

      sender ! FormAdding


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
            <li><a href="/open">/Display file</a></li>
            <li><a href="/addingName">/Add record</a></li>
            <li><a href="/findBy">/Find by</a></li>
            <li><a href="/edit">/Edit record</a></li>
            <li><a href="/removeName">/Remove record</a></li>
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

  lazy val FormEdit = HttpResponse (
    entity = HttpEntity(`text/html`,
      <html>
        <body>
          <h1>Find record you want to edit </h1>
          <form name="input" action="/edycja" method="post" />
          Name: <input type="text" name="name" /> <br/>
          Age: <input type="text" name="age" /> <br/>
          Sex: <input type="text" name="sex" /> <br/>
          Address: <input type="text" name="address" /> <br/>
          <input type="submit" value="Find" />
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
}*/
