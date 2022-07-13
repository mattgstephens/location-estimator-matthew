package phone

import parser.GenericParseCSVInternational

object InternationalPhoneClassifier extends App {

  def getRecommendation(phone_number: String): Option[String] = {

    // Change to correct file path!
    // Will probably have to move the next two lines into the generic parser, and take in the list
    // of country phone numbers as a second argument to getRecommendation!
    val phone_country_list: List[String] =
      GenericParseCSVInternational.parseCSVInternational(
        "src/main/resources/country_phone_numbers.csv"
      )

    val phone_map = phone_country_list
      .map(numberPair => numberPair.split(","))
      .map(splitPair => (splitPair(1).replace("-", ""), splitPair(0)))
      .toMap

    val user_phone: String = phone_number.replace("+", "")

    // Print line here!
    System.out.println("Passed in phone number: " + user_phone)

    // ANOTHER EDGE CASEEEE --> Need to think about what to do when you have
    // 0 as the first number?? Like when you have a 077 for instance (Zimbabwe)
    // Replacement code to handle the edge cases!
    val search_phone1 = user_phone
    phone_map.get(search_phone1) match {
      case Some(country) => Some(country)
      case None =>
        val search_phone2 = search_phone1.take(4)
        phone_map.get(search_phone2) match {
          case Some(country) => Some(country)
          case None =>
            val search_phone3 = search_phone2.take(3)
            phone_map.get(search_phone3) match {
              case Some(country) => Some(country)
              case None =>
                val search_phone4 = search_phone3.take(2)
                phone_map.get(search_phone4) match {
                  case Some(country) => Some(country)
                  case None =>
                    val search_phone5 = search_phone4.take(1)
                    phone_map.get(search_phone5) match {
                      case Some(country) => Some(country)
                      case None          => None
                    }
                }
            }
        }
    }
  }

  System.out.println(
    "Guessed country (edge case --> 1 number CC) : " + getRecommendation(
      "+14017431661"
    )
  )
  System.out.println("-------------------------------")
  System.out.println(
    "Guessed country (edge case --> 2 numbers CC) : " + getRecommendation(
      "+273712439594"
    )
  )
  System.out.println("-------------------------------")
  System.out.println(
    "Guessed country (edge case --> 3 numbers CC) : " + getRecommendation(
      "+263712439594"
    )
  )
  System.out.println("-------------------------------")
  System.out.println(
    "Guessed country (edge case --> 4 numbers CC) : " + getRecommendation(
      "+168413413413"
    )
  )
  System.out.println("-------------------------------")

}
