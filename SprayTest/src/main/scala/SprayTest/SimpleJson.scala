package SprayTest

import spray.json._
import DefaultJsonProtocol._ // !!! IMPORTANT, else `convertTo` and `toJson` won't work


case class Address(no: String, street: String, city: String)

case class Person(name: String, age: Int, sex: String, address: Address)


/*object MyJsonProtocol extends DefaultJsonProtocol{
  implicit val addressFormat = jsonFormat3(Address)
 // implicit val personFormat = jsonFormat1(Person)
}*/


object SimpleJson extends DefaultJsonProtocol{
  implicit val addressFormat = jsonFormat3(Address)
  implicit val personFormat = jsonFormat4(Person)

  def main(args: Array[String]) {
    val json = """{ "no": "A1", "street" : "Main Street", "city" : "Colombo" }"""
    val address = JsonParser(json).convertTo[Address]
    println(address)

    val json2 = """{ "name" : "John", "age" : 26,  "sex" : "Male" , "address" : { "no": "A1", "street" : "Main Street", "city" : "Colombo" }}"""

    val person = JsonParser(json2).convertTo[Person]
    println(person)
  }
}