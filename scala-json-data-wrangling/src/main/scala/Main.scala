import hw.json._
import hw.wrangling.WranglingLike

object Wrangling extends WranglingLike {

  val data: List[Json] = JsonHelper.fromFile("yelp.json")

  def key(json: Json, key: String): Option[Json] = json match {
    case JsonDict(aMap) => aMap.get(JsonString(key)) match {
      case None => None
      case _ => aMap.get(JsonString(key))
      }
    case _ => None
    }

  def fromState(data: List[Json], state: String): List[Json] = {
    data.filter(datum => isFromState(datum, state))
  }

// helper for fromState
  def isFromState(datum: Json, state: String): Boolean = datum match{
    case JsonDict(aMap) => aMap.get(JsonString("state")) match {
      case None => false
      case Some(stateName) => stateName match{
        case JsonString(x) => x == state
        case _ => false
        }
      }
      case _ => false 
    }

  def ratingLT(data: List[Json], rating: Double): List[Json] = {
    data.filter(datum => isLT(datum, rating))
    }

// helper for ratingLT
  def isLT(datum: Json, rating: Double): Boolean = datum match {
    case JsonDict(aMap) => aMap.get(JsonString("stars")) match {
      case None => false
      case Some(rate) => rate match{
        case JsonNumber(r) => r <= rating
        case _ => false
        }
      }
      case _ => false
    }

  def ratingGT(data: List[Json], rating: Double): List[Json] = {
    data.filter(datum => isGT(datum, rating))
    }

// helper for ratingGT
  def isGT(datum: Json, rating: Double): Boolean = datum match {
     case JsonDict(aMap) => aMap.get(JsonString("stars")) match {
      case None => false
      case Some(rate) => rate match{
        case JsonNumber(r) => r >= rating
        case _ => false
        }
      }
      case _ => false
    }

  def category(data: List[Json], category: String): List[Json] = {
    data.filter(datum => isCategory(datum, category))
    }

// helper methods for category
   def isCategory(datum: Json, category: String): Boolean = 
    key(datum, "categories") match {
      case Some(JsonArray(list)) => helperArray(helperList(list), category)
      case _ => false
         }

// also groupByCategory
  def helperList(list: List[Json]): List[String] = list match{
    case JsonString(head) :: tail => head :: helperList(tail)
    case _ => Nil
    }
  
  def helperArray(list: List[String], key: String): Boolean = list match {
    case h :: t => if(h == key) true else helperArray(t, key)
    case _ => false
    }
// ^    
// |

  def groupByState(data: List[Json]): Map[String, List[Json]] = data.groupBy(datum => datum match {
      case JsonDict(aMap) => aMap.get(JsonString("state")) match {
        case Some(JsonString(stateCode)) => stateCode
        case _ => "no state"
      }
      case _ => "no state"
    })
/*
  def groupByCategory(data: List[Json]): Map[String, List[Json]] = data.groupBy(datum => datum match {
    case JsonDict(map) => map.get(JsonString("categories")) match {
      case Some(JsonArray(list)) => traverseList(helperList(list))
      case _ => "no category"
        }
    case _ => "no category"
    })
*/

  def groupByCategory(data: List[Json]): Map[String, List[Json]] = 
data.map(dupCategory).flatten.groupBy(tuple => tuple._1).mapValues(data => data.map(tuple => tuple._2))

  def traverseList(list: List[String]): String = list match {
    case Nil => "no category"
    case head :: tail => tail match {
      case Nil => head
      case h :: t => traverseList(tail)
      }
    }

  def dupCategory(list: List[Json]): Map[String, List[Json]] =
    traverseList(helperList(list)).map(cat => (cat, list))

  /*
  data.groupBy(datum => datum match {
      case JsonDict(aMap) => aMap.get(JsonString("categories")) match {
        case Some(JsonArray(list)) => helperList(list) match {
            case Nil => "no category"
            case head :: tail => tail match {
              case Nil => head
              case h :: t => groupByCategory(tail)
              }
          }
        case _ => "no category"
        }
        case _ => "no category"
  // similar to groupByState
  })
*/


  def bestPlace(data: List[Json]): Option[Json] = data.sortWith(helperBestPlace) match {
      case Nil => None
      case head :: tail => key(head, "name")
    }

// helper methods for bestPlace
  def helperBestPlace(a: Json, b: Json): Boolean = (ratings(a) == ratings(b)) match {
      case true => (reviews(a) > reviews(b)) 
      case false => (ratings(a) > ratings(b)) 
    }      

  def ratings(datum: Json): Double = key(datum, "stars") match {
    case Some(JsonNumber(x)) => x
    case _ => 0
    }

  def reviews(datum: Json): Double = key(datum, "review_count") match {
      case Some(JsonNumber(x)) => x
      case _ => 0
      }
// ^
// |

  def hasAmbience(data: List[Json], ambience: String): List[Json] = {
    data.filter(datum => findAmbience(datum, ambience))
    }

// helper for hasAmbience
  def findAmbience(datum: Json, ambience: String): Boolean = key(datum, "attributes") match {
      case None => false
      case Some(JsonDict(map)) => map.get(JsonString("Ambience")) match {
        case Some(JsonDict(map2)) => map2.get(JsonString(ambience)) match {
          case Some(JsonBool(x)) => if(x) true else false
          case _ => false
          }                          
        case _ => false
        }
       case _ => false
      }

}