package phone

import parser.GenericParseCSVInternational

object InternationalPhoneClassifier extends App {

  /*
  Checks the basic CSV for the country. This will only work when
  the country code is given! Need to find patterns when the +country_code
  is not given.
   */
  def getRecommendationBasicCSV(phone_number: String): Option[String] = {

    val phone_country_list: List[String] =
      GenericParseCSVInternational.parseCSVInternational(
        "src/main/resources/country_phone_numbers.csv"
      )

    val phone_map = phone_country_list
      .map(numberPair => numberPair.split(","))
      .map(splitPair => (splitPair(1).replace("-", ""), splitPair(0)))
      .toMap

    val does_number_have_country_code: Boolean = {
      if (phone_number.charAt(0).equals('+')) true
      else false
    }

    if (does_number_have_country_code) {
      val user_phone: String = phone_number.replace("+", "").replace("-", "")

      val search_phone1 = user_phone
      phone_map.get(search_phone1) match {
        case Some(country) => Some(country)
        case None =>
          val search_phone2 = search_phone1.take(6)
          phone_map.get(search_phone2) match {
            case Some(country) => Some(country)
            case None =>
              val search_phone3 = search_phone2.take(5)
              phone_map.get(search_phone3) match {
                case Some(country) => Some(country)
                case None =>
                  val search_phone4 = search_phone3.take(4)
                  phone_map.get(search_phone4) match {
                    case Some(country) => Some(country)
                    case None =>
                      val search_phone5 = search_phone4.take(3)
                      phone_map.get(search_phone5) match {
                        case Some(country) => Some(country)
                        case None =>
                          val search_phone6 = search_phone5.take(2)
                          phone_map.get(search_phone6) match {
                            case Some(country) => Some(country)
                            case None =>
                              val search_phone7 = search_phone6.take(1)
                              phone_map.get(search_phone7) match {
                                case Some(country) => Some(country)
                                case None          => None
                              }
                          }
                      }
                  }
              }
          }
      }
    } else {
      None
    }
  }

  System.out.println(
    "Guessed country (edge case --> 1 number CC) : " + getRecommendationBasicCSV(
      "+14017431661"
    )
  )
  System.out.println("-------------------------------")
  System.out.println(
    "Guessed country (edge case --> 2 numbers CC) : " + getRecommendationBasicCSV(
      "+273712439594"
    )
  )
  System.out.println("-------------------------------")
  System.out.println(
    "Guessed country (edge case --> 3 numbers CC) : " + getRecommendationBasicCSV(
      "+263712439594"
    )
  )
  System.out.println("-------------------------------")
  System.out.println(
    "Guessed country (edge case --> 4 numbers CC) : " + getRecommendationBasicCSV(
      "+168413413413"
    )
  )
  System.out.println("-------------------------------")
  System.out.println(
    "Guessed country (edge case --> Same country for different country code) : " + getRecommendationBasicCSV(
      "+1-93993489238"
    )
  )
  System.out.println("-------------------------------")
  System.out.println(
    "Guessed country (edge case --> No country code (No plus sign)) : " + getRecommendationBasicCSV(
      "071274365235"
    )
  )

}
