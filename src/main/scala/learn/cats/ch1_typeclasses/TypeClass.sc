
// Page 10

// Define a very simple JSON AST
sealed trait Json
final case class JsObject(get: Map[String, Json]) extends Json
final case class JsString(get: String) extends Json
final case class JsNumber(get: Double) extends Json
case object JsNull extends Json

// The "serialize to JSON" behaviour is encoded in this trait
trait JsonWriter[A] {
  def write(value: A): Json
}

// 1.1.2 Type Class Instances
final case class Person(name: String, email: String)
object JsonWriterInstances {
  implicit val stringWriter: JsonWriter[String] =
    new JsonWriter[String] {
      def write(value: String): Json =
        JsString(value)
    }
  implicit val personWriter: JsonWriter[Person] =
    new JsonWriter[Person] {
      def write(value: Person): Json =
        JsObject(Map(
          "name" -> JsString(value.name),
          "email" -> JsString(value.email)
        ))
    }
  // etc...
}
// 1.1.3. Type Class Interfaces
object Json {
  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json =
    w.write(value)
}

import JsonWriterInstances._
Json.toJson(Person("Dave", "dave@example.com"))
// res4: Json = JsObject(Map(name -> JsString(Dave), email -> JsString
// (dave@example.com)))

Json.toJson(Person("Dave", "dave@example.com"))(personWriter)

// Page 12
// Interface syntax
object JsonSyntax {
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json =
      w.write(value)
  }
}

import JsonWriterInstances._
import JsonSyntax._
Person("Dave", "dave@example.com").toJson
// res6: Json = JsObject(Map(name -> JsString(Dave), email -> JsString
// (dave@example.com)))

Person("Dave", "dave@example.com").toJson(personWriter)

// Page 13
// The implicitly method
def implicitly[A](implicit value: A): A =
  value

import JsonWriterInstances._
// import JsonWriterInstances._

implicitly[JsonWriter[String]]
// res8: JsonWriter[String] = JsonWriterInstances$$anon$1@a5a4735

Json.toJson("A string")

// 1.2.2
// The following will intentionally give an error as
// Error:(79, 12) ambiguous implicit values:
//  both value stringWriter in object JsonWriterInstances of type => JsonWriter[String]
// and value writer1 of type => JsonWriter[String]
// match expected type JsonWriter[String]

//implicit val writer1: JsonWriter[String] =
//  JsonWriterInstances.stringWriter
//implicit val writer2: JsonWriter[String] =
//  JsonWriterInstances.stringWriter
//Json.toJson("A string")

// Page 16

// 1.2.3. Recursive Implicit Resolution using function to create

implicit def optionWriter[A]
(implicit writer: JsonWriter[A]): JsonWriter[Option[A]] =
  new JsonWriter[Option[A]] {
    def write(option: Option[A]): Json =
      option match {
        case Some(aValue) => writer.write(aValue)
        case None => JsNull
      }
  }

Json.toJson(Option("A string"))

Json.toJson(Option("A string"))(optionWriter(stringWriter))


