package email

import parser.GenericParseCSVInternational

object InternationalEmailClassifier extends App {

  def getRecommendation(email: String): Option[String] = {

    // Change to correct file path!
    // Will probably have to move the next two lines into the generic parser, and take in the list
    // of country phone numbers as a second argument to getRecommendation!
    val country_list: List[String] =
      GenericParseCSVInternational.parseCSVInternational(
        "src/main/resources/country_phone_numbers.csv"
      )

    val domain_map = country_list
      .map(countryPair => countryPair.split(","))
      .map(splitPair => (splitPair(0), splitPair(1)))
      .toMap

    // Get the last two letters of the passed in email to make it searchable.
    val user_domain: String = email.replace(".", "").takeRight(2)
    // Print line here!
    System.out.println(user_domain)
    val search_domain = "." + user_domain
    val result = domain_map.get(search_domain)
    result
  }

  System.out.println(getRecommendation("matthew@brown.co.uk"))
  System.out.println("-------------------------------")

  // NEED EDGE CASES!!!
  // What about the cases with weird characters?
  System.out.println(getRecommendation("matthew_stephens@brown.co.zw"))
  System.out.println("-------------------------------")
  System.out.println(getRecommendation("matthew@brown.co.za"))
  System.out.println("-------------------------------")
  System.out.println(getRecommendation("matthew@brown.co.fr"))
  System.out.println("-------------------------------")
  System.out.println(getRecommendation("matthew@brown.co.gr"))
  System.out.println("-------------------------------")
  System.out.println(getRecommendation("matthew@brown.co.hk"))
  System.out.println("-------------------------------")

}
