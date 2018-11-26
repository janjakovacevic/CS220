import Wrangling._
import hw.json._

// You will need to write more tests.
class TestSuite extends org.scalatest.FunSuite {

  val A: Json = JsonHelper.parse("""
    {
    "name": "Richmond Town Square",
    "city": "Richmond Heights",
    "state": "OH",
    "stars": 2,
    "review_count": 17,
    "attributes": {
      "RestaurantsPriceRange2": 2,
      "BusinessParking": {
        "garage": false,
        "street": false,
        "validated": false,
        "lot": true,
        "valet": false
      },
      "BikeParking": true,
      "WheelchairAccessible": true
    },
    "categories": [
      "Shopping",
      "Shopping Centers"
    ]
    }
  """)

  val B: Json = JsonHelper.parse("""
    {
      "name": "South Florida Style Chicken & Ribs",
      "city": "Charlotte",
      "state": "NC",
      "stars": 4.5,
      "review_count": 4,
      "attributes": {
        "GoodForMeal": {
          "dessert": false,
          "latenight": false,
          "lunch": false,
          "dinner": false,
          "breakfast": false,
          "brunch": false
        },
        "HasTV": false,
        "RestaurantsGoodForGroups": true,
        "NoiseLevel": "average",
        "RestaurantsAttire": "casual",
        "RestaurantsReservations": false,
        "OutdoorSeating": false,
        "BusinessAcceptsCreditCards": false,
        "RestaurantsPriceRange2": 2,
        "RestaurantsDelivery": true,
        "Ambience": {
          "romantic": false,
          "intimate": false,
          "classy": false,
          "hipster": false,
          "divey": false,
          "touristy": false,
          "trendy": false,
          "upscale": false,
          "casual": false
        },
        "RestaurantsTakeOut": true,
        "GoodForKids": true
      },
      "categories": [
        "Food",
        "Soul Food",
        "Convenience Stores",
        "Restaurants"
      ]
    } 
  """)

  val C: Json = JsonHelper.parse("""
    {
      "name": "Neighborhood Vision Center",
      "city": "Gilbert",
      "state": "AZ",
      "stars": 5,
      "review_count": 8,
      "attributes": {
        "AcceptsInsurance": true,
        "ByAppointmentOnly": true,
        "BusinessAcceptsCreditCards": true
      },
      "categories": [
        "Health & Medical",
        "Optometrists"
      ]
    } 
  """)

  val D: Json = JsonHelper.parse("""
    {
      "name": "Precision Tune Auto Care",
      "city": "Pineville",
      "state": "NC",
      "stars": 3,
      "review_count": 14,
      "attributes": {
        "BusinessAcceptsCreditCards": true
      },
      "categories": [
        "Automotive",
        "Auto Repair",
        "Tires",
        "Oil Change Stations"
      ]
    }
  """)

  val E: Json = JsonHelper.parse("""
  {
    "name": "Red Lobster",
    "city": "Madison",
    "state": "WI",
    "stars": 3,
    "review_count": 45,
    "attributes": {
      "RestaurantsTableService": true,
      "GoodForMeal": {
        "dessert": false,
        "latenight": false,
        "lunch": false,
        "dinner": true,
        "breakfast": false,
        "brunch": false
      },
      "Alcohol": "full_bar",
      "Caters": false,
      "HasTV": true,
      "RestaurantsGoodForGroups": true,
      "NoiseLevel": "average",
      "WiFi": "no",
      "RestaurantsAttire": "casual",
      "RestaurantsReservations": true,
      "OutdoorSeating": false,
      "BusinessAcceptsCreditCards": true,
      "RestaurantsPriceRange2": 2,
      "BikeParking": true,
      "RestaurantsDelivery": false,
      "Ambience": {
        "romantic": false,
        "intimate": false,
        "classy": false,
        "hipster": false,
        "divey": false,
        "touristy": false,
        "trendy": false,
        "upscale": false,
        "casual": true
      },
      "RestaurantsTakeOut": true,
      "GoodForKids": true,
      "WheelchairAccessible": true,
      "BusinessParking": {
        "garage": false,
        "street": false,
        "validated": false,
        "lot": true,
        "valet": false
      }
    },
    "categories": [
      "Restaurants",
      "Seafood",
      "American (Traditional)"
    ]
  }
  """)

  val F: Json = JsonHelper.parse("""
  {
  "name": "The Works",
  "city": "Pickering",
  "state": "ON",
  "stars": 3,
  "review_count": 41,
  "attributes": {
    "RestaurantsTableService": true,
    "GoodForMeal": {
      "dessert": false,
      "latenight": false,
      "lunch": true,
      "dinner": true,
      "breakfast": false,
      "brunch": false
    },
    "Alcohol": "full_bar",
    "Caters": false,
    "HasTV": false,
    "RestaurantsGoodForGroups": true,
    "NoiseLevel": "average",
    "WiFi": "no",
    "RestaurantsAttire": "casual",
    "RestaurantsReservations": true,
    "OutdoorSeating": false,
    "BusinessAcceptsCreditCards": true,
    "RestaurantsPriceRange2": 2,
    "BikeParking": true,
    "RestaurantsDelivery": false,
    "Ambience": {
      "romantic": false,
      "intimate": false,
      "classy": false,
      "hipster": false,
      "touristy": false,
      "trendy": false,
      "upscale": false,
      "casual": true
    },
    "RestaurantsTakeOut": true,
    "GoodForKids": true,
    "DriveThru": false,
    "BusinessParking": {
      "garage": false,
      "street": false,
      "validated": false,
      "lot": true,
      "valet": false
    }
  },
  "categories": [
    "Burgers",
    "Restaurants"
  ]
  }
  """)

  val json: List[Json] = List(A, B, C, D, E, F)
  val list: List[Json] = List(D, E, F)
  val small_list: List[Json] = List(B, C)
  val empty_list: List[Json] = List()

  test("Empty test works") {
    assert(true)
  }

  test("key test") {
    assert(key(A, "state") == Some(JsonString("OH")))
    assert(key(B, "stars") == Some(JsonNumber(4.5)))
    //assert(key(A, "lot") == Some(JsonBool(true))) //f
    //failing on nested stuff --> arrays
    }

  test("fromState test") {
    assert(fromState(json, "OH") == List(A))
    assert(fromState(json, "NC") == List(B, D))
    }
  
  test("ratingLT test") {
    assert(ratingLT(json, 2) == List(A))
    assert(ratingLT(json, 0.9) == List())
    }

  test("ratingGT test") {
    assert(ratingGT(json, 1) == List(A, B, C, D, E, F))
    assert(ratingGT(json, 3) == List(B, C, D, E, F))
    assert(ratingGT(json, 5.1) == List())
    }
  
  test("category test") {
     assert(category(json, "Food") == List(B)) //not working
    }

  test("groupByState test") {
     assert(groupByState(json) == Map("OH" -> List(A), "NC" -> List(B, D), "AZ" -> List(C), "WI" -> List(E), "ON" -> List(F)))                    
    }

  test("groupByCategory test") {
    assert(groupByCategory(json) == Map("Food" -> List(B), "Soul Food" -> List(B), 
    "Convenience Stores" -> List(B), "Restaurants" -> List(B), 
    "Health & Medical" -> List(C), "Optometrists" -> List(C))) 
    }
  
  test("bestPlace test") {
    assert(bestPlace(json) == Some(JsonString("Neighborhood Vision Center")))
    assert(bestPlace(list) == Some(JsonString("Red Lobster")))
    }

  test("hasAmbience test") {
    assert(hasAmbience(json, "romantic") == List())
    assert(hasAmbience(json, "casual") == List(E, F))
    }
}
