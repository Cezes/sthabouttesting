package Spray1


import java.io.{FileWriter, File}
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
import SprayTest.{MyJsonProtocol, Person}
import spray.json.JsonParser
import scala.collection.parallel.mutable
import scala.annotation.{Annotation, StaticAnnotation}
import scalax.io._



case class APIInfo(
                    description: String
                    ) extends StaticAnnotation
case class ParamInfo(
                      description: String
                      ) extends Annotation with StaticAnnotation

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
    (get | put) {
      path("") {
        complete(index)
      }~
      pathPrefix("plik") {
          path("open"){
            val source = scala.io.Source.fromFile("file.txt")
            val lines = source.mkString
            source.close()
            complete(lines)  : @APIInfo(description = "Showing contents of file")
          }~
            path("addingName"){
              respondWithMediaType(`text/html`)(complete(FormAdding)) : @APIInfo(description = "Showing form for adding new entry to file")
            }~
            path("removeName"){
              respondWithMediaType(`text/html`)(complete(FormRemove)) : @APIInfo(description = "Showing form for removing entry from file")
            }~
            path("findBy"){
           respondWithMediaType(`text/html`)(complete(FormFind)) : @APIInfo(description = "Showing form for finding entry based on specific criteria")
          }~
           path("edit"){
             respondWithMediaType(`text/html`)(complete(FormEdit)) : @APIInfo(description = "Showing form for editing entry in file")
           }~
            path(""){
            complete(fileOperations) : @APIInfo(description = "Showing all possible operations which can be performed on provided file")
          }
      }~
       path("stats") {
          complete {
            actorRefFactory.actorFor("/user/IO-HTTP/listener-0")
              .ask(Http.GetStats)(1.second)
              .mapTo[Stats]
          } : @APIInfo(description = "Showing statistics of server usage")
        } ~
        path("timeout") { ctx =>
          // we simply let the request drop to provoke a timeout
        } ~
        path("crash") { ctx =>
          sys.error("crash boom bang") : @APIInfo(description = "Crashing server on purpose")
        } ~
        path("fail") {
          failWith(new RuntimeException("aaaahhh")) : @APIInfo(description = "Server crashed")
        }
    } ~
      (post | parameter('method ! "post")) {
        path("stop") {
          complete {
            in(1.second){ actorSystem.shutdown() }
            "Shutting down in 1 second..."
          }   : @APIInfo(description = "Shutting down server on demand")
        }~
         pathPrefix("plik"){
            path("find"){
            formFields(
              'name ,
              'age,
              'sex ,
              'address )
            {
              (name, age, sex, address) =>
                var temp = 0
                if (age.isEmpty ){
                  temp = -1
                }
                else  { temp = age.toInt }
                var person = Person(name,temp,sex,address)
                var result = ""
                try{
                  var source = scala.io.Source.fromFile("file.txt")
                  for ( line <- source.getLines()){
                    var currentLineResult = findMatch( line, person)
                    result = result + currentLineResult
                    currentLineResult =""
                  }

                  source.close()
                }
                complete(result)  : @APIInfo(description = "Find entry based on specified criteria")
            }
        }~
              path("edite"){
                formFields(
                 'name,
                 'newName,
                 'age,
                 'newAge,
                 'sex,
                 'newSex,
                 'address,
                 'newAddress)
                {
                  (name , newName, age, newAge, sex, newSex, address, newAddress) =>

                    println("name " + name + " sex " + sex + " address " + address)
                    import MyJsonProtocol._
                    var temp = 0
                    if (age.isEmpty ){
                      temp = -1
                    }
                    else  { temp = age.toInt }
                    var temporary = 0
                    val file: Seekable =  Resource.fromFile("file.txt")
                    var position = 0
                    val personToEdit = Person(name, temp, sex, address)
                    //var result = ""
                   try{
                     //var source = scala.io.Source.fromFile("file.txt")
                     val fileLenght = file.lines().mkString.length
                     var offset = 0
                     for( line <- file.lines()){
                       println(line)
                       var currentLineResult = ""
                       if ( position + line.length <= fileLenght){
                          currentLineResult = findMatch( line, personToEdit)}
                       else{
                          currentLineResult = findMatch ( line.substring(0, (line.length-offset)), personToEdit )}
                       if ( currentLineResult.isEmpty){
                         position = position + line.length + 1
                       }
                       else {

                         if (newAge.isEmpty ){
                           temporary = -1
                         }
                         else  { temporary = newAge.toInt }

                         val newLine =  editPerson(line , Person(newName, temporary, newSex, newAddress))
                         file.patch(position , newLine , OverwriteSome(line.length))
                         file.string
                         if ( newLine.length > line.length)
                           offset = offset + (newLine.length - line.length)
                         position = position + newLine.length + 1

                       }
                       val source = scala.io.Source.fromFile("file.txt")
                       val lines = source.mkString
                       println( '\n' + lines + "\n")
                       source.close()

                     }
                     file.patch(position , "" , OverwriteAll)
                     // source.close()
                     //val pw = new java.io.PrintWriter(new File("file.txt"))
                     //pw.write(result)
                     //pw.close()
                     val source = scala.io.Source.fromFile("file.txt")
                     val lines = source.mkString
                     source.close()
                     complete(lines) : @APIInfo(description = "Edit file")
                   }
                }
              }~
        path("append"){
          formFields(
            'firstname ,
            'age,
            'sex ,
            'address )
          {
            (firstname, age, sex, address) =>

          val jsonToAppend = "{\"name\" : \"%s\", \"age\" : %s,  \"sex\" : \"%s\" , \"address\" : \"%s\"} ".format(firstname , age, sex, address)
          val file: Seekable =  Resource.fromFile("file.txt")
          file.append("\n" + jsonToAppend)
          val source = scala.io.Source.fromFile("file.txt")
          val lines = source.mkString
          source.close()
          complete(lines) : @APIInfo(description = "Adding new entry to file")
         }
        }
      } ~
       path("remove"){
           formFields('user) {
             (user) => {
           val nameToRemove = user
           val file: Seekable =  Resource.fromFile(new File("file.txt"))
           import MyJsonProtocol._
           var position = 0
           try{
             //var source = scala.io.Source.fromFile("file.txt")
             for ( line <- file.lines()){
               if (JsonParser(line).convertTo[Person].name.equals(nameToRemove)){
                  file.patch(position, "", OverwriteSome(line.length))
                  println(line)
               }  else {
                   position = position + line.length
               }
             }
           }
           val source = scala.io.Source.fromFile("file.txt")
           val lines = source.mkString
           source.close()
           complete(lines) : @APIInfo(description = "Removing entry from file")
         }
       }
    }
     }~
      (put | parameter('method ! "put")){

        complete("put")
      }~
      pathPrefix("api") {
      path("api-docs") {
        val source = scala.io.Source.fromFile("api-docs.json")
        val lines = source.mkString
        source.close()
        complete(lines) : @APIInfo(description = "Showing contents of json needed for SWAGGER")

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
          <form name="input" action="/plik/append" method="post">
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
          <form name="input" action="/plik/find" method="post" />
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
          <form name="input" action="/plik/edite" method="post" />
          Name: <input type="text" name="name" />
          New Name: <input type="text" name="newName" /> <br/>
          Age: <input type="text" name="age" />
          New Age: <input type="text" name="newAge" /><br/>
          Sex: <input type="text" name="sex" />
          New Sex: <input type="text" name="newSex" /><br/>
          Address: <input type="text" name="address" />
          New Address: <input type="text" name="newAddress" /><br/>
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

  def findMatch(line : String ,  personToFind : Person ) : String = {
    import MyJsonProtocol._
    var currentLine =  JsonParser(line).convertTo[Person]
    var currentLineResult =  line + "\n"

    //var string =""
    if (personToFind.name == "" ){
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

  def editPerson(line : String ,  personToEdit : Person ) : String = {
    import MyJsonProtocol._
    var currentLine =  JsonParser(line).convertTo[Person]
    println(personToEdit)
    var name = personToEdit.name
    var age = personToEdit.age
    var sex = personToEdit.sex
    var address = personToEdit.address
    //var string =""
    println(personToEdit)
    if (personToEdit.name.isEmpty ){
       name = currentLine.name
    }
    if (personToEdit.age ==  -1 ) {
       age = currentLine.age
    }
    if (personToEdit.sex.isEmpty  ){
       sex = currentLine.sex
       println("Edit person" + currentLine)
    }
    if (personToEdit.address.isEmpty  ){
       address = currentLine.address
    }
    "{\"name\" : \"%s\", \"age\" : %s,  \"sex\" : \"%s\" , \"address\" : \"%s\"} ".format(name, age, sex, address)
  }
  def appendFile(fileName: String, line: String) = {
    val fw = new FileWriter(fileName , true) ;
    fw.write( line + "\n") ;
    fw.close()
  }
}
