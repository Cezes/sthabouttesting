package SprayTest


import spray.httpx.unmarshalling.{Unmarshaller, pimpHttpEntity}
import spray.json._
import DefaultJsonProtocol._
import spray.http._
import spray.httpx.marshalling._


/*case class Address(no: String, street: String, city: String)*/



case class Person(name: String, age: Int, sex: String, address: String)

object MyJsonProtocol extends DefaultJsonProtocol {
  implicit val PersonFormat = jsonFormat4(Person.apply)
}


object Person {
  val `application/x-www-form-urlencoded` = MediaTypes.register(MediaType.custom("application/x-www-form-urlencoded"))


  implicit val PersonMarshaller = Marshaller.of[Person](`application/x-www-form-urlencoded`) {(value, contentType, ctx) =>
    val Person(name, age, sex, address) = value
    val string = " {\"firstname\" : \"%s\", \"age\" : %s,  \"sex\" : \"%s\" , \"address\" : \"%s\"} ".format(name , age, sex, address)
    //val string = "firstname=%s&age=%s&sex=%s&address=%s".format(name , age, sex, address)
    ctx.marshalTo(HttpEntity(contentType, string))

  }
/*
  implicit val PersonUnmarshaller =
    Unmarshaller[Person](`application/x-www-form-urlencoded`) {
      case HttpBody(contentType, buffer ) =>
        // unmarshal from the string format used in the marshaller example
        val Array(_ , name : String , _, age : String , _, sex : String, _, address : String)  =
          buffer.toString.split("=&".toCharArray)
         println("akakska")
         Person(name, age.toInt, sex, address)
      case EmptyEntity =>
        println("zzz")
        Person("ala", 24, "Female", "sdsdsdds")
      case _ =>
        println("xx")
        Person("ala", 24, "Female", "sdsdsdds")

       //val string = " {\"firstname\" : \"%s\", \"age\" : %s,  \"sex\" : \"%s\" , \"address\" : \"%s\"} ".format(name , age, sex, address)

    }
*/



/*  def main(args: Array[String]) {

   // val p = marshal(Person("Bob", 32, "Male", "adsasdasdafs")

    //val p = Person("Bob", 32, "Male", "adsasdasdafs"

    import MyJsonProtocol._

    val json = """{ "name" : "John", "age" : 26,  "sex" : "Male" , "address" : "asdasd" }"""
    val p = JsonParser(json).convertTo[Person]
    println(p)
    val g = marshal(p)*/

    //  val body = HttpEntity(`application/x-www-form-urlencoded`,"firstname=Danirel&age=24&sex=Male&address=Jakistam")
    //println(body.getClass )
//    val p = body.as[Person]
   // val g = marshal(p)
   // print(g)
   // println(p.getString(p))


   /* val p = marshal(Person("Bob", 32, "Male", "adsasdasdafs"))
    println( p.as[Person])*/
/*    val body = HttpEntity("application/vnd.acme.person", "Person: Bob, 32, Male, adsasui")
    body.as[Person] === Right(Person("Bob", "Parr", 32))


    val json = """{ "no": "A1", "street" : "Main Street", "city" : "Colombo" }"""
    val address = JsonParser(json).convertTo[Address]
    println(address)

    val json2 = """{ "name" : "John", "age" : 26,  "sex" : "Male" , "address" : { "no": "A1", "street" : "Main Street", "city" : "Colombo" }}"""

    val person = JsonParser(json2).convertTo[Person]
    println(person)*/
  //}
}