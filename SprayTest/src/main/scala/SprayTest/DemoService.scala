
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
import SprayTest.Person
import StatusCodes._
import Directives._
import scalax.file.Path



class DemoServiceActor extends Actor with DemoService {

  def actorRefFactory = context
  def receive = runRoute(demoRoute)
}

trait DemoService extends HttpService with SprayTest.MyJsonProtocol {

  implicit def executionContext = actorRefFactory.dispatcher

  val demoRoute = {
    handleRejections(myHandler) {
      allowCrossDomain {
        post{
          path("file"){
            entity(as[String])  { json  =>
              val person = JsonParser(json).convertTo[Person]
              //Sprawdzenie poprawnosci wprowadzonych do formularza danych
              if( !(person.sex.equals("male") | person.sex.equals("female")) | person.name.isEmpty  | person.address.isEmpty )
                complete(BadRequest, "Bad parameters used.")
              else{
                if(person.age < 0 )
                  complete(BadRequest, "Bad parameters used.")
                else{
                  //val person: Person = Person(name.toLowerCase , personAge, gender, address)
                  //sprawdzenie czy imie jest unikalne
                  if (findIfNameIsUnique(Person(person.name,-1,"",""))){
                    var source = scala.io.Source.fromFile("file.txt")
                    var lines = source.mkString
                    source.close()
                    val file: Seekable = Resource.fromFile("file.txt")
                    //Sprawdzenie czy nie dopisujemy juz do isteniejacych danych , jesli tak to dajemy znak mowej lini na poczatek
                    if (lines != "")
                      file.append("\n" + PersonFormat.write(person))
                    else
                      file.append(PersonFormat.write(person).toString())
                    source = scala.io.Source.fromFile("file.txt")
                    lines = source.mkString
                    source.close
                    complete(lines.toString)
                  } else
                    complete(BadRequest, "Name must be unique.")
                }


              }                                      }
          }
        }~
          delete{
            path("file"){
              val path : Path = Path("remove.txt")
              path.deleteIfExists()
              complete("ok")
            }
          }~
          (get | put | parameter('method ! "put" )) {
            path("") {
              complete(index)
            }~
              path("mystyle.css"){
                val source = scala.io.Source.fromFile("mystyle.css")
                val lines = source.mkString
                source.close
                complete(lines)
              }~
              path("mystyle2.css"){
                val source = scala.io.Source.fromFile("mystyle2.css")
                val lines = source.mkString
                source.close
                complete(lines)
              }~
              path("file" / "\\w+".r) { name =>
                get{
                  var person = Person(name.toString,-1,"","")
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
                  complete(result)
                }~
                  put{
                    var personToEdit = Person(name,-1,"","")

                    var temporary = 0
                    val file: Seekable = Resource.fromFile("file.txt")
                    var position = 0

                    try{
                      val fileLenght = file.lines().mkString.length
                      var offset = 0
                      for( line <- file.lines()){
                        var currentLineResult = ""
                        if ( position + line.length <= fileLenght){
                          currentLineResult = findMatch( line, personToEdit)}
                        else{
                          //offset wprowadzony aby pozbyc sie buga w ktorym jesli nowa linijka była krotsza to w ostatniej linijsce pliku bylo przesuniecie
                          val linesubstring = line.substring(0, (line.length - offset))
                          if ( linesubstring.isEmpty == false)
                            currentLineResult = findMatch ( line.substring(0, (line.length-offset)), personToEdit )}
                        if ( currentLineResult.isEmpty){
                          position = position + line.length + 1
                        }
                        else {
                          // import SprayTest.MyJsonProtocol
                          val currPerson = JsonParser(line).convertTo[Person]
                          val newPerson = Person(name, currPerson.age+1, currPerson.sex, currPerson.address)
                          val newLine =  editPerson(line , newPerson)
                          if ( newLine.length > line.length)
                            offset = offset + (newLine.length - line.length)
                          file.patch(position  , newLine , OverwriteSome(line.length))
                          file.string
                          position = position + newLine.length + 1
                        }
                      }
                    }
                    val source = scala.io.Source.fromFile("file.txt")
                    val lines = source.mkString
                    source.close()
                    complete(lines.toString)
                  }
              }~
              pathPrefix("plik") {
                path("open"){
                  val source = scala.io.Source.fromFile("file.txt")
                  val lines = source.mkString
                  source.close()
                  complete(lines)
                }~
                  path("addingName"){
                    respondWithMediaType(`text/html`)(complete(FormAdding))
                  }~
                  path("removeName"){
                    respondWithMediaType(`text/html`)(complete(FormRemove))
                  }~
                  path("findBy"){
                    respondWithMediaType(`text/html`)(complete(FormFind))
                  }~
                  path("edit"){
                    respondWithMediaType(`text/html`)(complete(FormEdit))
                  }~
                  path("find"){

                    parameter('name ,
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
                        complete(result)
                    }
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
              pathPrefix("plik"){
                path("append"){
                  formFields(
                    'firstname ,
                    'age,
                    'sex ,
                    'address )
                  {
                    (firstname, age, sex, address) =>

                      val gender = sex.toLowerCase
                      if( !(gender.equals("male") | gender.equals("female")) | firstname.isEmpty | age.isEmpty | address.isEmpty )
                        complete(BadRequest, "Bad parameters used.")
                      else{
                        var personAge = 0
                        try {
                          personAge =age.toInt
                        } catch {
                          case ex: NumberFormatException =>{
                            personAge = -1
                          }
                        }
                        if(personAge < 0 )
                          complete(BadRequest, "Age must be a number higher or equal 0")
                        else{
                          val person  = Person(firstname , personAge, gender, address)
                          if (findIfNameIsUnique(Person(firstname, -1, "",""))){
                            val PersonFormat = jsonFormat(Person, "name", "age", "sex", "address")
                            val file: Seekable = Resource.fromFile("file.txt")
                            file.append("\n" + PersonFormat.write(person))
                            val source = scala.io.Source.fromFile("file.txt")
                            val lines = source.mkString
                            source.close()
                            complete(lines)
                          } else
                            complete(BadRequest, "Name must be unique")
                        }
                      }
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
                        var temp = 0
                        var personAge = 0
                        if (age.isEmpty ){
                          temp = -1
                        }
                        else  {
                          try {
                            personAge =age.toInt
                          } catch {
                            case ex: NumberFormatException =>{
                              personAge = -1
                            }
                          }
                        }
                        val gender = sex.toLowerCase
                        val newGender = newSex.toLowerCase
                        if(personAge < 0 | !(gender.equals("male") | gender.equals("female") | gender.isEmpty) | !(newGender.equals("male") | newGender.equals("female") | newGender.isEmpty))
                          if (personAge < 0)
                            complete(BadRequest, "Age must be a number higher or equal 0")
                          else
                            complete(BadRequest, "Wrong sex parameter use \"male\" or \"female\"")
                        else if ( findIfNameIsUnique(Person(newName, -1,"",""))){
                          if (temp == 0){
                            temp = age.toInt
                          }
                          var temporary = 0
                          val file: Seekable = Resource.fromFile("file.txt")
                          var position = 0
                          val personToEdit = Person(name, temp, sex, address)
                          try{
                            val fileLenght = file.lines().mkString.length
                            var offset = 0
                            for( line <- file.lines()){
                              var currentLineResult = ""
                              if ( position + line.length <= fileLenght){
                                currentLineResult = findMatch( line, personToEdit)}
                              else{
                                val linesubstring = line.substring(0, (line.length - offset))
                                if ( linesubstring.isEmpty == false)
                                  currentLineResult = findMatch ( line.substring(0, (line.length-offset)), personToEdit )}
                              if ( currentLineResult.isEmpty){
                                position = position + line.length + 1
                              }
                              else {

                                if (newAge.isEmpty ){
                                  temporary = -1
                                }
                                else  {
                                  try {
                                    personAge =newAge.toInt
                                  } catch {
                                    case ex: NumberFormatException =>{
                                      personAge = -1
                                    }
                                  }
                                }
                                if (personAge < 0 ){
                                  complete(BadRequest, "new Age parameter must be a number higher or equal 0")
                                } else {
                                  if (temporary == 0){
                                    temporary = newAge.toInt
                                  }

                                  val newLine =  editPerson(line , Person(newName, temporary, newSex, newAddress))
                                  file.patch(position , newLine , OverwriteSome(line.length))
                                  file.string
                                  if ( newLine.length > line.length)
                                    offset = offset + (newLine.length - line.length)
                                  position = position + newLine.length + 1
                                }
                              }
                            }
                            val source = scala.io.Source.fromFile("file.txt")
                            val lines = source.mkString
                            source.close()
                            complete(lines)
                          }
                        }   else{
                          complete(BadRequest, "New name must be unique")
                        }
                    }
                  }~
                  pathPrefix("remove"){
                    formFields('user) {
                      (nameToRemove) => {
                        if ( nameToRemove.isEmpty){
                          complete(BadRequest, "Bad name")
                        } else{
                          val file: Seekable = Resource.fromFile(new File("file.txt"))
                          var position = 0
                          try{
                            for ( line <- file.lines()){
                              if (JsonParser(line).convertTo[Person].name.equals(nameToRemove)){
                                file.patch(position, "", OverwriteSome(line.length))
                              }  else {
                                position = position + line.length
                              }
                            }
                          }
                          val source = scala.io.Source.fromFile("file.txt")
                          val lines = source.mkString
                          source.close()
                          complete(lines)
                        }
                      }
                    }
                  }
              }
          }~
          pathPrefix("api") {
            path("api-docs.json") {
              val source = scala.io.Source.fromFile("api-docs.json")
              val lines = source.mkString
              source.close()
              complete(lines)

            }
          }
      }
    }
  }

 // lazy vals for forms, html pages
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
        <link  type="text/css" href="\mystyle.css" rel="stylesheet" ></link>
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
      <head>
        <link type="text/css" href="\mystyle.css" rel="stylesheet"></link>
      </head>
      <body>
        <h1>Remove from file</h1>
        <form name="input" action="/plik/remove" method="post" >
          <div id ="formWrapper">

            <label for="user"> Username: </label>
            <input type="text" placeholder ="Username" name="user" />
            <br/>

            <input type="submit" value="Remove" />
          </div>
        </form>
      </body>
    </html>


  lazy val FormFind =
    <html xmlns="http://www.w3.org/1999/xhtml" lang="pl" xml:lang="pl" >
      <head>
        <link type="text/css" href="\mystyle.css" rel="stylesheet" ></link>
      </head>
      <body>
        <h1>Find by </h1>
        <form name="input" action="/plik/find" method="get">
          <div id ="formWrapper">

            <label for="name">Name:</label>
            <input type="text" placeholder="Name" name="name" />
            <br/>

            <label for="age">Age:</label>
            <input type="text" placeholder="age" name="age" />
            <br/>

            <label for="sex">Sex:</label>
            <input type="text" placeholder="sex" name="sex" />
            <br/>

            <label for="address">Address:</label>
            <input type="text" placeholder="address" name="address" />
            <br/>

            <input type="submit" value="Find" />
            <br/>
          </div>
        </form>
      </body>
    </html>

  lazy val FormEdit =
    <html xmlns="http://www.w3.org/1999/xhtml" lang="pl" xml:lang="pl" >
      <head>
        <link type ="text/css" href="\mystyle2.css" rel="stylesheet"></link>
      </head>
      <body>
        <h1>Find record you want to edit </h1>
        <form name="input" action="/plik/edite" method="post">
          <div id="formWrapper">

            <label for="name">Name:</label>
            <input type="text" placeholder="name" name="name" />

            <label for="newName" > New Name:</label>
            <input type="text" placeholder="new Name" name="newName" />
            <br/>

            <label for="age" > Age:</label>
            <input type="text" placeholder="age" name="age" />

            <label for="newAge"  > New Age  :</label>
            <input type="text" placeholder="new age" name="newAge" />
            <br/>

            <label for="sex">Sex:</label>
            <input type="text" placeholder="sex" name="sex" />

            <label for="newSex">New sex:</label>
            <input type="text" placeholder="new sex" name="newSex" />
            <br/>

            <label for="address">Address:</label>
            <input type="text" placeholder="address" name="address" />

            <label for="newAddress">New Address:</label>
            <input type="text" placeholder="new Address" name="newAddress" />
            <br/>

            <input type="submit" value="Edit" />
            <br/>
          </div>
        </form>
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

  /**
   * Auxiliary method which checks if passed name in form is unique
   * @param personToFind
   * @return
   */
  def findIfNameIsUnique (personToFind: Person) : Boolean = {
    var isUnique = true
    val source = scala.io.Source.fromFile("file.txt")
    for( line <- source.getLines()){
      if ( !(findMatch(line, personToFind).isEmpty))
        isUnique = false
    }
    isUnique
  }

  /**
   * Auxiliary method for finding matches in file
   * it searches for specific Person type based fields
   * that were passed in  findform
   * @param line from resource file (in json format)
   * @param personToFind
   * @return
   */
  def findMatch(line : String ,  personToFind : Person ) : String = {

    var currentLine =  JsonParser(line).convertTo[Person]
    var currentLineResult =  line + "\n"

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

  /**
   * Method which takes as parameter new Person and line in which this person can be found in json file
   * Returns either empty String, which there was no such Person in file
   * or json of Person
   * @param line
   * @param personToEdit
   * @return
   */
  def editPerson(line : String ,  personToEdit : Person ) : String = {
    var currentLine =  JsonParser(line).convertTo[Person]
    var name = personToEdit.name
    var age = personToEdit.age
    var sex = personToEdit.sex
    var address = personToEdit.address
    if (personToEdit.name.isEmpty ){
      name = currentLine.name
    }
    if (personToEdit.age ==  -1 ) {
      age = currentLine.age
    }
    if (personToEdit.sex.isEmpty  ){
      sex = currentLine.sex
    }
    if (personToEdit.address.isEmpty  ){
      address = currentLine.address
    }
    PersonFormat.write(Person(name, age, sex, address)).toString()
  }

}
